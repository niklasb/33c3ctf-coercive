#!/bin/bash
set -xe
cd ~/coercive
git fetch --all
git reset --hard origin/master
stack build

rm -rf tmp
mkdir -p tmp
chmod 711 tmp

scripts/stop.sh || true
screen -dmS coercive stack exec coercive
screen -dmS coercive-cleaner sudo /home/coercive/coercive/scripts/cleanup.sh
