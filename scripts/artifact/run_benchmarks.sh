#!/bin/bash

# If not enough arguments are given
if [ -z "$1" ]
then
echo "Please provide an output path (for the results)"
exit 1
fi

docker run --rm -v $1:/app/results -it scam-scv-2022-artifact performance
docker run --rm -v $1:/app/results -it scam-scv-2022-artifact precision

echo "Benchmarks have finished"
