#!/bin/sh

./cabal/init.sh db
./cabal/init.sh back
./web-app/init.sh
