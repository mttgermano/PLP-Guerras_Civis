#!/bin/sh

# Init the Database
docker-compose -f ./database/docker-compose.yml up &

# Init the Poject
sudo cabal v2-build && sudo cabal run