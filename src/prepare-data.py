#!/usr/bin/env python3
import json

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


def assign_colors(neighbours):
    """Country code -> colour, clearly different from its neighbours' colours.

    Colours countries with the most coloured neighbours first (DSATUR). Of the
    colours clearly different from all its neighbours', each country gets the
    least used one, to use the whole palette evenly. If there's none, it gets
    the one furthest from its neighbours'.
    """
    colors = {}
    usage = {c: 0 for c in PALETTE}

    def constraint(code):
        coloured = [n for n in neighbours[code] if n in colors]
        return (len(coloured), len(neighbours[code]), code)

    def distance(color, code):
        return min(
            (delta_e(color, colors[n]) for n in neighbours[code] if n in colors),
            default=100,
        )

    while len(colors) < len(neighbours):
        code = max((c for c in neighbours if c not in colors), key=constraint)
        color = max(
            PALETTE,
            key=lambda c: (
                min(distance(c, code), MIN_NEIGHBOUR_DIFFERENCE),
                -usage[c],
            ),
        )
        colors[code] = color
        usage[color] += 1

    return colors


def report_colors(neighbours, colors, names):
    pairs = sorted(
        (delta_e(colors[a], colors[b]), names[a], names[b])
        for a in neighbours
        for b in neighbours[a]
        if a < b
    )
    print(f"{len(pairs)} neighbouring pairs, closest colours (ΔE2000):")
    for d, a, b in pairs[:5]:
        print(f"  {d:5.1f}  {a} – {b}")


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
    colors = assign_colors(neighbours)
    report_colors(neighbours, colors, dict(zip(data["ADM0_A3"], data["NAME"])))

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
