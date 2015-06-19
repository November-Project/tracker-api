#!/bin/sh
#
# usage: ./bin/deploy [APP]
#
#   APP defaults to carnival-staging
#
#   Set BUILD=1 to perform a Halcyon build
#
###
set -e

case "${1:-staging}" in
  staging)
    cabal clean
    cabal configure -fproduction
    cabal build
    tar cfvz /app/dist/build/slug/slug.tgz -C / --exclude=.git ./app
    # Upload to Heroku?
    ;;
  production)
    heroku pipeline:promote --app novproject-staging
    heroku restart --app novproject-production
    ;;
  *)
    sed '/^# \(usage:.*\)/!d; s//\1/' "$0" >&2
    exit 64
    ;;
esac

