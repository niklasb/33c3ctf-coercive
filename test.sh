#!/bin/bash
cd exploit
if python2 solve.py test | grep 33C3_; then
  echo YES
  exit 0
else
  echo NO
  exit 1
fi
