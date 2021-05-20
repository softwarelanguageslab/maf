#!/bin/sh
echo "=== RQ3 ==="
echo "Producing ModF metrics for 0CFA (expected time: 45min)"
START_METRICS_0CFA=$(date +"%T")
sbt 'maf/runMain maf.cli.experiments.parallel.ParallelMetrics0CFA' > out-metrics.txt
END_METRICS_0CFA=$(date +"%T")
echo "Started at $START_METRICS_0CFA, ended at $END_METRICS_0CFA"

echo "Producing ModF metrics for 1CFA (expected time: 45min)"
START_METRICS_1CFA=$(date +"%T")
sbt 'maf/runMain maf.cli.experiments.parallel.ParallelMetrics1CFA' > out-metrics.txt
END_METRICS_1CFA=$(date +"%T")
echo "Started at $START_METRICS_1CFA, ended at $END_METRICS_1CFA"

echo "Producing ModF metrics for 2CFA (expected time: 45min)"
START_METRICS_2CFA=$(date +"%T")
sbt 'maf/runMain maf.cli.experiments.parallel.ParallelMetrics2CFA' >> out-metrics.txt
END_METRICS_2CFA=$(date +"%T")
echo "Started at $START_METRICS_2CFA, ended at $END_METRICS_2CFA"

echo "Producing data for 0CFA (expected time: 2h)"
START_DATA_0CFA=$(date +"%T")
sbt 'maf/runMain maf.cli.experiments.parallel.ParallelPerformanceMetrics0CFA' > out-metrics-perf.txt
END_DATA_0CFA=$(date +"%T")
echo "Started at $START_DATA_0CFA, ended at $END_DATA_0CFA"

echo "Producing data for 1CFA (expected time: 2h)"
START_DATA_1CFA=$(date +"%T")
sbt 'maf/runMain maf.cli.experiments.parallel.ParallelPerformanceMetrics1CFA' >> out-metrics-perf.txt
END_DATA_1CFA=$(date +"%T")
echo "Started at $START_DATA_1CFA, ended at $END_DATA_1CFA"

echo "Producing data for 2CFA (expected time: 2h)"
START_DATA_2CFA=$(date +"%T")
sbt 'maf/runMain maf.cli.experiments.parallel.ParallelPerformanceMetrics2CFA' >> out-metrics-perf.txt
END_DATA_2CFA=$(date +"%T")
echo "Started at $START_DATA_2CFA, ended at $END_DATA_2CFA"

echo "To produce the graph, run:"
echo "python modf-metrics-context-insensitive-plots.py"
echo "To find correlations, run:"
echo "python findcorrelations.py"

# TODO: reproduce for 2CFA (same script)
# For 2CFA, expected time:
# Metrics: 3h
# Analysis: 6h
