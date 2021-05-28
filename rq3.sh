#!/bin/sh
# echo "=== RQ3 ==="

echo "Producing data for 0CFA (expected time: 2h)"
START_DATA_0CFA=$(date +"%T")
#sbt 'maf/runMain maf.cli.experiments.parallel.ParallelPerformanceMetrics0CFA' > out-metrics-perf.txt
END_DATA_0CFA=$(date +"%T")
echo "Started at $START_DATA_0CFA, ended at $END_DATA_0CFA"

echo "Producing data for 1CFA (expected time: 13h)"
START_DATA_1CFA=$(date +"%T")
#sbt 'maf/runMain maf.cli.experiments.parallel.ParallelPerformanceMetrics1CFA' >> out-metrics-perf.txt
END_DATA_1CFA=$(date +"%T")
echo "Started at $START_DATA_1CFA, ended at $END_DATA_1CFA"

echo "Producing data for 2CFA (expected time: 14h)"
START_DATA_2CFA=$(date +"%T")
#sbt 'maf/runMain maf.cli.experiments.parallel.ParallelPerformanceMetrics2CFA' >> out-metrics-perf.txt
END_DATA_2CFA=$(date +"%T")
echo "Started at $START_DATA_2CFA, ended at $END_DATA_2CFA"

echo "Finished!"
