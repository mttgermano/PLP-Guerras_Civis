#!/bin/sh

# Init the Database
docker-compose -f ./database/docker-compose.yml up

# Init the Poject
cabal v2-build && cabal run
