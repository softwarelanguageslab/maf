#!/bin/sh
echo "=== RQ3 ==="
echo "Producing ModF metrics for 0CFA (expected time: 30min)"
START_METRICS_0CFA=$(date +"%T")
sbt 'maf/runMain maf.cli.experiments.parallel.ParallelMetrics0CFA'
END_METRICS_0CFA=$(date +"%T")
echo "Started at $START_METRICS_0CFA, ended at $END_METRICS_0CFA"

echo "Producing data for 0CFA (expected time: TODO, started on $(date))"
START_DATA_0CFA=$(date +"%T")
sbt 'maf/runMain maf.cli.experiments.parallel.ParallelPerformanceMetrics0CFA'
END_DATA_0CFA=$(date +"%T")
echo "Started at $START_DATA_0CFA, ended at $END_DATA_0CFA"

echo "To produce the graph, run:"
echo "python modf-metrics-context-insensitive-plots.py"
echo "To find correlations, run:"
echo "python findcorrelations.py"

# TODO: reproduce for 2CFA (same script)
