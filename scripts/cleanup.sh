#!/bin/bash
cd /home/coercive/coercive
while sleep 300; do
  echo "Deleting old files"
  find tmp/ -mmin +5 \( -type f -o -type l -o -type d \) -a -not -path tmp/ -exec rm -rf {} \;
done
