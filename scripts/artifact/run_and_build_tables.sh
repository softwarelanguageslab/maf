#!/bin/bash

if [ -z "$1" ]
then
   echo "Please provide an output folder for the results of the benchmark"
   exit 1
fi

echo "Running benchmarks"
./scripts/artifact/run_benchmarks.sh $@

echo "Processing benchmarks and producing tables"

./scripts/artifact/produce_tables.sh $@
