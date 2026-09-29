#!/bin/sh

set -e

if [ $# -ne 2 ]; then
    echo Usage: $0 REPO TOKEN
    exit 1
fi

curl -X DELETE https://ci.appveyor.com/api/projects/$1/buildcache \
     -H "Authorization: Bearer $2" \
     -H "Content-Type: application/json"
