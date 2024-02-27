#!/bin/sh

# Init the Database
docker-compose -f ./database/docker-compose.yml up

# Init the Poject
cabal build && cabal run
