#!/bin/sh
echo "=== RQ3 ==="
echo "Producing ModF metrics for 0CFA (expected time: 1h, started on $(date))"
sbt 'maf/runMain maf.cli.experiments.parallel.ParallelMetrics0CFA'
echo "Producing data for 0CFA (expected time: TODO, started on $(date))"
sbt 'maf/runMain maf.cli.experiments.parallel.ParallelPerformanceMetrics0CFA'

echo "Finished at $(date)"
echo "To produce the graph, run:"
echo "python modf-metrics-context-insensitive-plots.py"
echo "To find correlations, run:"
echo "python findcorrelations.py"

# echo "Producing ModF metrics for 2CFA (expected time: TODO, started on $(date))"
# sbt 'maf/runMain maf.cli.experiments.parallel.ParallelMetrics2CFA'
# echo "Producing data for 0CFA (expected time: TODO, started on $(date))"
# sbt 'maf/runMain maf.cli.experiments.parallel.ParallelPerformanceMetrics2CFA'
# echo "Finished, to the graph, run: python modf-metrics-context-insensitive-plots.py"
