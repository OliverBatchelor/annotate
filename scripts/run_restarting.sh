#!/bin/bash
while `true`
do
  ./server $@
  echo "Restarting in 5..."
  sleep 5
done
