#!/bin/sh
echo "Setting up JVM with 64GB of heap size"
sed -i 's/-Xmx4096M/-Xmx65536M/' .jvmopts

echo "=== RQ1 ==="
echo "Running base ModF results (expected time: 15h30, started on $(date), output done on out.log)"
sbt 'maf/runMain maf.cli.experiments.parallel.BaseResultsModF' > out.log

echo "Producing speedup data for 0CFA (expected time: 5h00, started on $(date))"
sbt 'maf/runMain maf.cli.experiments.parallel.ParallelModFPerformance0CFA' >> out.log

echo "Producing speedup data for 1CFA (expected time: 5h30, started on $(date))"
sbt 'maf/runMain maf.cli.experiments.parallel.ParallelModFPerformance1CFA' >> out.log

echo "Producing speedup data for 2CFA (expected time: 12h30, started on $(date))"
sbt 'maf/runMain maf.cli.experiments.parallel.ParallelModFPerformance2CFA' >> out.log

echo "Finished!"
