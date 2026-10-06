#!/usr/bin/env python3
import json
import math
import re
from functools import cache

import geopandas as gpd
import pandas as pd
from shapely.geometry import MultiPolygon, Polygon

from color_difference import delta_e

# Natural Earth comes in three scales. Each is generalised consistently across
# all countries, so shared borders still line up (unlike simplifying each
# country's polygons independently).
RESOLUTIONS = {
    "10m": 4,  # 1e-4° ≈ 11 m
    "50m": 3,  # 1e-3° ≈ 110 m
    "110m": 2,  # 1e-2° ≈ 1.1 km
}


# Inoffensive pastels: similar lightness, low saturation. Some are close to
# each other, but neighbouring countries get clearly different ones (see
# assign_colors).
PALETTE = [
    "#9dc3c2",
    "#a7c8a0",
    "#c5ca91",
    "#e0cfa3",
    "#e1b6a0",
    "#d4a3a3",
    "#c4a3b5",
    "#b4a3c6",
    "#a3aad0",
    "#a3bfd8",
    "#92bccc",
    "#8fbfb8",
    "#a1c1a9",
    "#c2c2a3",
    "#d3b7a3",
    "#c6aba3",
    # lighter and darker ones, filling gaps between the above
    "#d9d4eb",
    "#f0d0d0",
    "#cbdcc8",
    "#b09f75",
    "#99a67b",
]

# Countries with a fixed colour, outside the palette: country code -> colour.
FIXED_COLORS = {
    "ATA": "#ffffff",  # Antarctica, white as ice
}

# Countries closer than this count as neighbours: besides land borders, this
# includes narrow straits (Dover, Gibraltar, Øresund...), where countries look
# adjacent on the map.
NEIGHBOUR_DISTANCE = 0.3  # degrees, ≈ 30 km

# Colour difference (ΔE2000) at which neighbouring countries are clearly
# distinguishable.
MIN_NEIGHBOUR_DIFFERENCE = 14

# Nearby countries that aren't neighbours should preferably have different
# colours too. How near counts is the gap between them relative to the size of
# the smaller one, as country sizes vary so much: Canada and Mexico are 16°
# apart, but that's about Mexico's size, while 16° is all of Europe. Pairs
# with weight exp(-relative gap) below this are ignored.
MIN_NEARBY_WEIGHT = 0.05

# Countries sharing a neighbour (siblings) should preferably have different
# colours too, as on the map they surround the same country. Their weight is
# this, plus their nearby weight if they're also nearby.
SIBLING_WEIGHT = 1.0


def find_neighbours(data):
    """Country code -> set of codes of neighbouring countries."""
    neighbours = {code: set() for code in data["ADM0_A3"]}
    pairs = data.sindex.query(
        data.geometry, predicate="dwithin", distance=NEIGHBOUR_DISTANCE
    )
    for i, j in zip(*pairs):
        if i != j:
            neighbours[data["ADM0_A3"].iloc[i]].add(data["ADM0_A3"].iloc[j])
    return neighbours


def find_nearby(data, neighbours):
    """Country code -> {code of a nearby, non-neighbouring country: weight},
    the weight from 1 (touching) to 0 (far away, relative to their sizes)."""
    codes = list(data["ADM0_A3"])
    geometries = list(data.geometry.simplify(0.05))
    sizes = [math.sqrt(g.area) for g in geometries]
    nearby = {code: {} for code in codes}
    max_relative_gap = -math.log(MIN_NEARBY_WEIGHT)
    # Generous candidates: two large countries can be far apart and still near.
    pairs = data.sindex.query(data.geometry, predicate="dwithin", distance=30)
    for i, j in zip(*pairs):
        a, b = codes[i], codes[j]
        if a >= b or b in neighbours[a]:
            continue
        size = min(sizes[i], sizes[j])
        if size == 0:
            continue
        relative_gap = geometries[i].distance(geometries[j]) / size
        if relative_gap < max_relative_gap:
            weight = math.exp(-relative_gap)
            nearby[a][b] = weight
            nearby[b][a] = weight
    return nearby


def find_siblings(neighbours):
    """Country code -> codes of countries sharing a neighbour with it."""
    return {
        code: {
            c
            for n in neighbours[code]
            for c in neighbours[n]
            if c != code and c not in neighbours[code]
        }
        for code in neighbours
    }


def assign_colors(neighbours, nearby):
    """Country code -> colour, clearly different from its neighbours' colours.

    Colours countries with the most coloured neighbours first (DSATUR). Of the
    colours clearly different from all its neighbours', each country gets the
    one least similar to those of its siblings (countries sharing a neighbour)
    and nearby countries, weighted: SIBLING_WEIGHT plus how near they are (see
    MIN_NEARBY_WEIGHT). Remaining ties go to the least used colour overall, to
    use the whole palette. If no colour is clearly different from the
    neighbours', it gets the one furthest from them. Then each country's colour
    is improved, given all others. Countries in FIXED_COLORS keep theirs.
    """
    colors = dict(FIXED_COLORS)
    usage = {c: 0 for c in PALETTE}
    difference = cache(delta_e)
    siblings = find_siblings(neighbours)
    # Country code -> {code of a sibling or nearby country: weight}
    others = {
        code: {
            c: SIBLING_WEIGHT * (c in siblings[code]) + nearby[code].get(c, 0)
            for c in siblings[code] | nearby[code].keys()
        }
        for code in neighbours
    }

    def constraint(code):
        coloured = [n for n in neighbours[code] if n in colors]
        return (len(coloured), len(neighbours[code]), code)

    def neighbour_difference(color, code):
        return min(
            (difference(color, colors[n]) for n in neighbours[code] if n in colors),
            default=100,
        )

    def similarity(color, code):
        """How similar a colour is to those of siblings and nearby countries.

        Cubed, so that look-alike colours count much more than ones that are
        merely not clearly different: with only 21 colours, many siblings can't
        be clearly different, but they needn't look alike."""
        return sum(
            weight
            * max(0, 1 - difference(color, colors[c]) / MIN_NEIGHBOUR_DIFFERENCE) ** 3
            for c, weight in others[code].items()
            if c in colors
        )

    def best_color(code):
        return max(
            PALETTE,
            key=lambda c: (
                min(neighbour_difference(c, code), MIN_NEIGHBOUR_DIFFERENCE),
                -similarity(c, code),
                # when improving: only change for a better colour
                c == colors.get(code),
                -usage[c],
            ),
        )

    while len(colors) < len(neighbours):
        code = max((c for c in neighbours if c not in colors), key=constraint)
        colors[code] = best_color(code)
        usage[colors[code]] += 1

    # Countries coloured early only took those coloured before them into
    # account. Improve: recolour each with the best colour given all others,
    # until nothing changes.
    for _ in range(20):
        changed = False
        for code in sorted(neighbours.keys() - FIXED_COLORS.keys()):
            usage[colors[code]] -= 1
            color = best_color(code)
            usage[color] += 1
            changed |= color != colors[code]
            colors[code] = color
        if not changed:
            break

    return colors


# Only for reporting: colours this similar look alike.
LOOK_ALIKE = 8


def report_colors(neighbours, nearby, colors, names):
    pairs = sorted(
        (delta_e(colors[a], colors[b]), names[a], names[b])
        for a in neighbours
        for b in neighbours[a]
        if a < b
    )
    print(f"{len(pairs)} neighbouring pairs, closest colours (ΔE2000):")
    for d, a, b in pairs[:5]:
        print(f"  {d:5.1f}  {a} – {b}")

    siblings = find_siblings(neighbours)
    look_alikes = sorted(
        (
            -(SIBLING_WEIGHT * (b in siblings[a]) + nearby[a].get(b, 0)),
            delta_e(colors[a], colors[b]),
            names[a],
            names[b],
        )
        for a in neighbours
        for b in siblings[a] | nearby[a].keys()
        if a < b and delta_e(colors[a], colors[b]) < LOOK_ALIKE
    )
    print(
        f"{len(look_alikes)} pairs of siblings or nearby countries with "
        f"look-alike colours (ΔE2000 < {LOOK_ALIKE}), "
        f"total weight {-sum(w for w, *_ in look_alikes):.1f}, worst:"
    )
    for w, d, a, b in look_alikes[:6]:
        print(f"  weight {-w:.2f}  ΔE {d:4.1f}  {a} – {b}")


# Capitals where Natural Earth's populated places don't give a good answer:
# countries with several capitals (it doesn't say reliably what each is for,
# and may mark a former one as the capital), outdated ones, and dependencies
# whose capital it doesn't mark at all. A note in parentheses, e.g. what the
# capital is for, is shown with the name.
CAPITALS = {
    "ZAF": [
        "Pretoria (executive)",
        "Cape Town (legislative)",
        "Bloemfontein (judicial)",
    ],
    "BOL": ["Sucre (constitutional)", "La Paz (seat of government)"],
    "NLD": ["Amsterdam (constitutional)", "The Hague (seat of government)"],
    "MYS": ["Kuala Lumpur (official)", "Putrajaya (administrative)"],
    "LKA": ["Sri Jayawardenepura Kotte (legislative)", "Colombo (executive)"],
    "BEN": ["Porto-Novo (official)", "Cotonou (seat of government)"],
    "CIV": ["Yamoussoukro (official)", "Abidjan (seat of government)"],
    "SWZ": ["Mbabane (administrative)", "Lobamba (legislative)"],
    "CHL": ["Santiago", "Valparaíso (legislative)"],
    "BDI": ["Gitega (political)", "Bujumbura (economic)"],
    "MMR": ["Naypyidaw"],
    "TZA": ["Dodoma"],
    "KAZ": ["Astana"],
    "PLW": ["Ngerulmud"],
    # separately shown parts of countries
    "GUF": ["Cayenne"],
    "MTQ": ["Fort-de-France"],
    "GLP": ["Basse-Terre"],
    "REU": ["Saint-Denis"],
    "MYT": ["Mamoudzou"],
    "PAZ": ["Ponta Delgada"],
    "PMD": ["Funchal"],
    "NSV": ["Longyearbyen"],
    "NLY": ["Kralendijk"],
    "TZZ": ["Zanzibar City"],
    "CCK": ["West Island"],
    "CXR": ["Flying Fish Cove"],
    "PSX": ["Ramallah (administrative)"],
    "CYN": ["North Nicosia"],
    "PRI": ["San Juan"],
    "FRO": ["Tórshavn"],
    "GRL": ["Nuuk"],
    "JEY": ["Saint Helier"],
    "GGY": ["Saint Peter Port"],
    "VIR": ["Charlotte Amalie"],
    "VGB": ["Road Town"],
    "SXM": ["Philipsburg"],
    "MAF": ["Marigot"],
    "AIA": ["The Valley"],
    "BLM": ["Gustavia"],
    "SPM": ["Saint-Pierre"],
    "MSR": ["Brades (de facto)"],
    "SHN": ["Jamestown"],
    "WLF": ["Mata-Utu"],
    "NRU": ["Yaren (de facto)"],
    "MNP": ["Saipan"],
    "COK": ["Avarua"],
    "NIU": ["Alofi"],
    "NFK": ["Kingston"],
    "PCN": ["Adamstown"],
    "SGS": ["King Edward Point"],
}

# Coordinates (lat, lon) of capitals in CAPITALS that aren't in Natural
# Earth's populated places.
CAPITAL_LOCATIONS = {
    "Basse-Terre": (16.00, -61.73),
    "Saint-Denis": (-20.88, 55.45),
    "Mamoudzou": (-12.78, 45.23),
    "Kralendijk": (12.15, -68.27),
    "Zanzibar City": (-6.16, 39.20),
    "West Island": (-12.19, 96.83),
    "Flying Fish Cove": (-10.42, 105.68),
    "Longyearbyen": (78.22, 15.55),
    "Astana": (51.17, 71.43),
    "Ngerulmud": (7.50, 134.62),
    "North Nicosia": (35.18, 33.36),
    "Saint Helier": (49.19, -2.11),
    "Saint Peter Port": (49.46, -2.54),
    "Charlotte Amalie": (18.34, -64.93),
    "Road Town": (18.43, -64.62),
    "Philipsburg": (18.03, -63.05),
    "Marigot": (18.07, -63.08),
    "The Valley": (18.22, -63.05),
    "Gustavia": (17.90, -62.85),
    "Saint-Pierre": (46.78, -56.18),
    "Brades": (16.79, -62.21),
    "Jamestown": (-15.92, -5.72),
    "Mata-Utu": (-13.28, -176.17),
    "Yaren": (-0.55, 166.92),
    "Saipan": (15.19, 145.75),
    "Kingston": (-29.06, 167.96),
    "Adamstown": (-25.07, -130.10),
    "King Edward Point": (-54.28, -36.49),
}

# Parts of countries shown separately, e.g. French Guiana (France): overseas
# parts and outlying islands, by their code in Natural Earth's map units.
# Not internal regions such as Scotland or Flanders. They keep the colour of
# their country.
SEPARATE_PARTS = {
    # France
    "GUF",  # French Guiana
    "MTQ",  # Martinique
    "GLP",  # Guadeloupe
    "REU",  # Réunion
    "MYT",  # Mayotte
    # Portugal
    "PAZ",  # Azores
    "PMD",  # Madeira
    # Norway
    "NSV",  # Svalbard
    "NJM",  # Jan Mayen
    "BVT",  # Bouvet Island
    # others
    "NLY",  # Caribbean Netherlands
    "TKL",  # Tokelau (New Zealand)
    "TZZ",  # Zanzibar (Tanzania)
    "CCK",  # Cocos Islands (Australia)
    "CXR",  # Christmas Island (Australia)
    "PFA",  # Paracel Islands (China)
    # US Minor Outlying Islands
    "JQI",  # Johnston Atoll
    "DQI",  # Jarvis Island
    "FQI",  # Baker Island
    "HQI",  # Howland Island
    "WQI",  # Wake Atoll
    "MQI",  # Midway Islands
    "BQI",  # Navassa Island
    "LQI",  # Palmyra Atoll
    "KQI",  # Kingman Reef
}

# Natural Earth's populated places use a different code for some countries.
PLACE_CODES = {
    "SDS": "SSD",  # South Sudan
}


def find_capitals(data, parts):
    """Country or part code -> its capitals: [{name, note, lat, lon}], maybe
    none."""
    places = gpd.read_file("data/populated-places/ne_10m_populated_places_simple.shp")

    def places_of(code, feature_class=None):
        code = PLACE_CODES.get(code, code)
        found = places[places["adm0_a3"] == code]
        if feature_class:
            found = found[found["featurecla"] == feature_class]
        return found

    def capital(name, note, lat, lon):
        return {"name": name, "note": note, "lat": round(lat, 4), "lon": round(lon, 4)}

    def from_place(place):
        return capital(place["name"], None, place["latitude"], place["longitude"])

    def from_table(codes, entry):
        name, note = re.fullmatch(r"(.*?)(?: \((.*)\))?", entry).groups()
        if name in CAPITAL_LOCATIONS:
            return capital(name, note, *CAPITAL_LOCATIONS[name])
        place = pd.concat(places_of(code) for code in codes)
        place = place[place["name"] == name]
        assert len(place) == 1, f"{name}: add it to CAPITAL_LOCATIONS"
        return {**from_place(place.iloc[0]), "note": note}

    capitals = {}
    for code in data["ADM0_A3"]:
        capital_places = places_of(code, "Admin-0 capital")
        # Dependencies' capitals, e.g. Nuuk for Greenland
        region_capitals = places_of(code, "Admin-0 region capital")
        if code in CAPITALS:
            capitals[code] = [from_table([code], entry) for entry in CAPITALS[code]]
        elif len(capital_places) == 1:
            capitals[code] = [from_place(capital_places.iloc[0])]
        elif len(region_capitals) == 1:
            capitals[code] = [from_place(region_capitals.iloc[0])]
        else:
            capitals[code] = []
    for _, part in parts.iterrows():
        # Their capitals are in their country's places, e.g. Cayenne in France's
        codes = [part["GU_A3"], part["ADM0_A3"]]
        capitals[part["GU_A3"]] = [
            from_table(codes, entry) for entry in CAPITALS.get(part["GU_A3"], [])
        ]
    return capitals


def get_ring(coords, digits):
    ring = []
    for lon, lat in coords:
        point = [round(lon, digits), round(lat, digits)]
        if not ring or ring[-1] != point:
            ring.append(point)
    return ring


def get_polygons(polygons, digits):
    """A list of polygons, each a list of rings: exterior first, then holes."""
    result = []
    for p in polygons:
        rings = [get_ring(p.exterior.coords, digits)]
        rings.extend(get_ring(hole.coords, digits) for hole in p.interiors)
        result.append([r for r in rings if len(r) >= 3])

    return [p for p in result if p]


def find_parts():
    """The map units in SEPARATE_PARTS, with geometries at the most detailed
    resolution: also used to find them at the other resolutions."""
    units = gpd.read_file("data/map-units/ne_10m_admin_0_map_units.shp")
    return units[units["GU_A3"].isin(SEPARATE_PARTS)]


def split_parts(country, parts):
    """A country's polygons: {part code (or None for the rest): polygons}."""
    geometry = country["geometry"]
    polygons = [geometry] if isinstance(geometry, Polygon) else geometry.geoms
    # a bit larger, as shapes differ between resolutions
    areas = [
        (part["GU_A3"], part["geometry"].buffer(0.2))
        for _, part in parts[parts["ADM0_A3"] == country["ADM0_A3"]].iterrows()
    ]
    split = {}
    for polygon in polygons:
        point = polygon.representative_point()
        code = next((code for code, area in areas if area.contains(point)), None)
        split.setdefault(code, []).append(polygon)
    return split


def area(polygons):
    """In km², in an equal-area projection."""
    series = gpd.GeoSeries([MultiPolygon(polygons)], crs="EPSG:4326")
    return series.to_crs("EPSG:6933").area.iloc[0] / 1e6


def iso_a2(code):
    # -99 for some disputed areas
    return None if code == "-99" else code


def main():
    # The most detailed data has all countries (the others have subsets), so
    # colours are the same at every resolution.
    data = gpd.read_file("data/admin-countries/ne_10m_admin_0_countries.shp")
    neighbours = find_neighbours(data)
    nearby = find_nearby(data, neighbours)
    colors = assign_colors(neighbours, nearby)
    report_colors(neighbours, nearby, colors, dict(zip(data["ADM0_A3"], data["NAME"])))
    parts = find_parts()
    capitals = find_capitals(data, parts)
    # e.g. United Republic of Tanzania -> Tanzania
    sovereign_names = dict(zip(data["ADMIN"], data["NAME_LONG"]))

    for resolution, digits in RESOLUTIONS.items():
        data = gpd.read_file(
            f"data/admin-countries/ne_{resolution}_admin_0_countries.shp"
        )

        countries = []
        for _, country in data.iterrows():
            code = country["ADM0_A3"]
            common = {
                # to show a country's parts together
                "group": code,
                "fill": colors[code],
            }
            for part_code, polygons in split_parts(country, parts).items():
                if part_code:
                    part = parts[parts["GU_A3"] == part_code].iloc[0]
                    info = {
                        "name": part["NAME"],
                        "long_name": part["NAME_LONG"],
                        "part_of": sovereign_names.get(
                            part["SOVEREIGNT"], part["SOVEREIGNT"]
                        ),
                        "iso_a2": iso_a2(part["ISO_A2_EH"]),
                        "capitals": capitals[part_code],
                        "population": int(part["POP_EST"]) or None,
                        "population_year": int(part["POP_YEAR"]),
                    }
                else:
                    info = {
                        "name": country["NAME"],
                        "long_name": country["NAME_LONG"],
                        "part_of": None,
                        "iso_a2": iso_a2(country["ISO_A2_EH"]),
                        "capitals": capitals[code],
                        "population": int(country["POP_EST"]) or None,
                        "population_year": int(country["POP_YEAR"]),
                    }
                countries.append(
                    {
                        **info,
                        **common,
                        "area": round(area(polygons)),  # km²
                        "polygons": get_polygons(polygons, digits),
                    }
                )

        with open(f"assets/countries-{resolution}.json", "w") as f:
            json.dump(countries, f, separators=(",", ":"))


if __name__ == "__main__":
    main()
