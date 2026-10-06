#!/usr/bin/env python3
"""Downloads the Natural Earth data that prepare-data.py uses, into data/.

Run it to update the data, then run prepare-data.py."""

import io
import urllib.request
import zipfile
from pathlib import Path

BASE_URL = "https://naciscdn.org/naturalearth"

# directory in data/ -> datasets
DATASETS = {
    "admin-countries": [
        "10m/cultural/ne_10m_admin_0_countries",
        "50m/cultural/ne_50m_admin_0_countries",
        "110m/cultural/ne_110m_admin_0_countries",
    ],
    "map-units": [
        "10m/cultural/ne_10m_admin_0_map_units",
    ],
    "populated-places": [
        "10m/cultural/ne_10m_populated_places_simple",
    ],
}


def main():
    for directory, datasets in DATASETS.items():
        target = Path("data") / directory
        target.mkdir(parents=True, exist_ok=True)
        for dataset in datasets:
            url = f"{BASE_URL}/{dataset}.zip"
            print(f"Downloading {url}")
            with urllib.request.urlopen(url) as response:
                archive = zipfile.ZipFile(io.BytesIO(response.read()))
            archive.extractall(target)


if __name__ == "__main__":
    main()
