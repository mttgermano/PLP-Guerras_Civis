#!/bin/sh

# Setup the cabal version for latest with `gchup tui`
# If it's your first time running, run:
    # sudo cabal install 

# Init the Poject
function back_start(){
    sudo cabal v2-build 
    sudo cabal run
}

# Init the Database
function db_start(){
    BASEPATH="./database/docker-compose.yml"
    docker-compose -f $BASEPATH down
    docker-compose -f $BASEPATH build --no-cache
    docker-compose -f $BASEPATH up
}

if [[ $#  -eq 0 ]]; then
    echo "-------------------------"
    echo "Wrong Use"
    echo "You must give a parameter"
    echo ""
    echo "Parameters aviable:"
    echo "db    - init the database"
    echo "back  - init the backed"
    echo "-------------------------"

#Updating Files
elif [[ $1 == "db" ]]; then
    db_start
elif [[ $1 == "back" ]]; then
    back_start
fi
