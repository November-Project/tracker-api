#!/bin/sh

docker-compose run web /app/bin/deploy.sh staging
docker run -iv $HOME:/ -v $PWD/dist/build/slug:/slug -t thoughtbot/heroku novproject-staging ./dist/build/novproject/novproject

