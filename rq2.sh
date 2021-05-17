#!/bin/sh
echo "Setting up JVM with 64GB of heap size"
sed -i 's/-Xmx4096M/-Xmx65536M/' .jvmopts

echo "=== RQ2 ==="
echo "Producing ModConc base results and speedup data (expected time: TODO, started on $(date))"
sbt 'maf/runMain maf.cli.experiments.parallel.ParallelPerformanceModConc'
echo "Finished, to get the graphs, run: python modconc_plot.py"
