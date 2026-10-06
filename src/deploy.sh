#!/usr/bin/env bash
# Builds the app and publishes it on GitHub Pages: pushes it to the gh-pages
# branch of the GitHub remote, replacing what's there.
#
#   src/deploy.sh [--no-push] [remote]
#
# --no-push only builds the site, in build/gh-pages. remote defaults to origin.
set -euo pipefail

cd "$(dirname "$0")/.."

push=true
if [[ "${1:-}" == "--no-push" ]]; then
    push=false
    shift
fi
remote="${1:-origin}"

if [[ -n "$(git status --porcelain)" ]]; then
    echo "Warning: there are uncommitted changes, which will be deployed too." >&2
fi

npx shadow-cljs release main

site=build/gh-pages
version=$(git rev-parse --short HEAD)
rm -rf "$site"
mkdir -p "$site/css" "$site/js/generated"
# the version makes browsers load the new files after a deploy
sed "s/__GIT-COMMIT-HASH__/$version/g" assets/index.html > "$site/index.html"
cp assets/css/styles.css "$site/css/"
cp assets/countries-*.json "$site/"
cp build/release/js/generated/main.js "$site/js/generated/"
# serve the files as they are, without GitHub's Jekyll processing
touch "$site/.nojekyll"

if [[ "$push" == false ]]; then
    echo "Built the site in $site"
    exit 0
fi

git -C "$site" init --quiet --initial-branch gh-pages
git -C "$site" add --all
git -C "$site" commit --quiet --message "Deploy $version"
git -C "$site" push --force "$(git remote get-url "$remote")" gh-pages
echo "Deployed $version to the gh-pages branch of $remote"
