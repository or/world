#!/usr/bin/env python3
import json

import geopandas as gpd
from shapely.geometry.polygon import Polygon

# Natural Earth comes in three scales. Each is generalised consistently across
# all countries, so shared borders still line up (unlike simplifying each
# country's polygons independently).
RESOLUTIONS = {
    "10m": 4,  # 1e-4° ≈ 11 m
    "50m": 3,  # 1e-3° ≈ 110 m
    "110m": 2,  # 1e-2° ≈ 1.1 km
}


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
    for resolution, digits in RESOLUTIONS.items():
        data = gpd.read_file(
            f"data/admin-countries/ne_{resolution}_admin_0_countries.shp"
        )

        countries = []
        for _, country in data.iterrows():
            new_country = {
                "name": country["NAME"],
                "label_position": [country["LABEL_X"], country["LABEL_Y"]],
                "polygons": get_polygons(country, digits),
            }
            countries.append(new_country)

        with open(f"assets/countries-{resolution}.json", "w") as f:
            json.dump(countries, f, separators=(",", ":"))


if __name__ == "__main__":
    main()
