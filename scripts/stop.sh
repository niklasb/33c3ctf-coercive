#!/bin/bash
killall -9 coercive
screen -X -S coercive quit
screen -X -S coercive-cleaner quit
