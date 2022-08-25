#!/bin/bash 

if [ -z "$1" ] 
then 
   echo "Please provide an output location" 
   exit 1
fi

docker run --rm -v $1:/app/results --entrypoint python3 -it scam-scv-2022-artifact scripts/Python/scv/performance/compare.py
docker run --rm -v $1:/app/results --entrypoint python3 -it scam-scv-2022-artifact scripts/Python/scv/precision/false_positives.py
