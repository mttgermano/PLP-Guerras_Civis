#!/bin/sh

# Init the Database
docker-compose -f ./database/docker-compose.yml up &

# Setup the cabal version for latest with `gchup tui`
# If it's your first time running, run:
    # sudo cabal install 

# Init the Poject
sudo cabal v2-build && sudo cabal run
