#!/usr/bin/env python3
import json

import geopandas as gpd
from shapely.geometry.polygon import Polygon


def get_polygons(country):
    poly = country["geometry"]

    if isinstance(poly, Polygon):
        polygon = [point for point in poly.exterior.coords]
        return [polygon]

    polygons = []
    for p in poly.geoms:
        polygon = [point for point in p.exterior.coords]
        polygons.append(polygon)

    return polygons


def main():
    data = gpd.read_file("data/admin-countries/ne_50m_admin_0_countries.shp")

    countries = []
    for _, country in data.iterrows():
        new_country = {
            "name": country["NAME"],
            "label_position": [country["LABEL_X"], country["LABEL_Y"]],
            "polygons": get_polygons(country),
        }
        countries.append(new_country)

    json.dump(countries, open("assets/countries.json", "w"))


if __name__ == "__main__":
    main()
