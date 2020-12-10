#!/bin/bash
# sbt fastOptJS # Can be used separately (this is faster in IntelliJ since it keeps sbt running where it should otherwise restart every time this script is run).
# sbt fullOptJS # Full optimisation for in-production code.
open -a safari ./maf.html