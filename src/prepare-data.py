#!/usr/bin/env python3
import json
import math
from functools import cache

import geopandas as gpd
from shapely.geometry.polygon import Polygon

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


def assign_colors(neighbours, nearby):
    """Country code -> colour, clearly different from its neighbours' colours.

    Colours countries with the most coloured neighbours first (DSATUR). Of the
    colours clearly different from all its neighbours', each country gets the
    one least used by nearby countries, weighted by how near they are (see
    MIN_NEARBY_WEIGHT). Similar colours count too. Remaining
    ties go to the least used colour overall, to use the whole palette. If no
    colour is clearly different from the neighbours', it gets the one furthest
    from them. Then each country's colour is improved, given all others.
    """
    colors = {}
    usage = {c: 0 for c in PALETTE}
    difference = cache(delta_e)

    def constraint(code):
        coloured = [n for n in neighbours[code] if n in colors]
        return (len(coloured), len(neighbours[code]), code)

    def neighbour_difference(color, code):
        return min(
            (difference(color, colors[n]) for n in neighbours[code] if n in colors),
            default=100,
        )

    def nearby_similarity(color, code):
        return sum(
            weight * max(0, 1 - difference(color, colors[c]) / MIN_NEIGHBOUR_DIFFERENCE)
            for c, weight in nearby[code].items()
            if c in colors
        )

    def best_color(code):
        return max(
            PALETTE,
            key=lambda c: (
                min(neighbour_difference(c, code), MIN_NEIGHBOUR_DIFFERENCE),
                -nearby_similarity(c, code),
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
        for code in sorted(neighbours):
            usage[colors[code]] -= 1
            color = best_color(code)
            usage[color] += 1
            changed |= color != colors[code]
            colors[code] = color
        if not changed:
            break

    return colors


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

    similar = sorted(
        (
            (-weight, delta_e(colors[a], colors[b]), names[a], names[b])
            for a, others in nearby.items()
            for b, weight in others.items()
            if a < b and delta_e(colors[a], colors[b]) < MIN_NEIGHBOUR_DIFFERENCE
        ),
    )
    print(
        f"{sum(len(o) for o in nearby.values()) // 2} nearby pairs, "
        f"{len(similar)} with similar colours (ΔE2000 < "
        f"{MIN_NEIGHBOUR_DIFFERENCE}), "
        f"total weight {-sum(w for w, *_ in similar):.1f}, nearest:"
    )
    for w, d, a, b in similar[:6]:
        print(f"  weight {-w:.2f}  ΔE {d:4.1f}  {a} – {b}")


def get_ring(coords, digits):
    ring = []
    for lon, lat in coords:
        point = [round(lon, digits), round(lat, digits)]
        if not ring or ring[-1] != point:
            ring.append(point)
    return ring


def get_polygons(country, digits):
    """A list of polygons, each a list of rings: exterior first, then holes."""
    geometry = country["geometry"]
    polys = [geometry] if isinstance(geometry, Polygon) else geometry.geoms

    polygons = []
    for p in polys:
        rings = [get_ring(p.exterior.coords, digits)]
        rings.extend(get_ring(hole.coords, digits) for hole in p.interiors)
        polygons.append([r for r in rings if len(r) >= 3])

    return [p for p in polygons if p]


def main():
    # The most detailed data has all countries (the others have subsets), so
    # colours are the same at every resolution.
    data = gpd.read_file("data/admin-countries/ne_10m_admin_0_countries.shp")
    neighbours = find_neighbours(data)
    nearby = find_nearby(data, neighbours)
    colors = assign_colors(neighbours, nearby)
    report_colors(neighbours, nearby, colors, dict(zip(data["ADM0_A3"], data["NAME"])))

    for resolution, digits in RESOLUTIONS.items():
        data = gpd.read_file(
            f"data/admin-countries/ne_{resolution}_admin_0_countries.shp"
        )

        countries = []
        for _, country in data.iterrows():
            new_country = {
                "name": country["NAME"],
                "fill": colors[country["ADM0_A3"]],
                "label_position": [country["LABEL_X"], country["LABEL_Y"]],
                "polygons": get_polygons(country, digits),
            }
            countries.append(new_country)

        with open(f"assets/countries-{resolution}.json", "w") as f:
            json.dump(countries, f, separators=(",", ":"))


if __name__ == "__main__":
    main()
