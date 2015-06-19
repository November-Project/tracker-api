#!/bin/sh

set -e

echo "Creating slug..."
slug=$(curl -X POST \
-H 'Content-Type: application/json' \
-H 'Accept: application/vnd.heroku+json; version=3' \
-d '{"process_types":{"web":"./dist/build/novproject/novproject"}}' \
-n https://api.heroku.com/apps/novproject-staging/slugs)

blobURL=$(echo "$slug" | jq -r .blob.url)
slugID=$(echo "$slug" | jq -r .id)

echo "Uploading slug..."
curl -X PUT \
-H "Content-Type:" \
--data-binary @slug.tgz \
"$blobURL"

echo "Releasing slug..."
curl -X POST \
-H "Accept: application/vnd.heroku+json; version=3" \
-H "Content-Type: application/json" \
-d "{\"slug\":\"$slugID\"}" \
-n https://api.heroku.com/apps/novproject-staging/releases
