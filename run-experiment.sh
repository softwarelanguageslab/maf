#!/bin/bash

sbt "maf/runMain maf.cli.experiments.domain.DomainSize benchmark"

SCRIPTDIR=$(dirname "$0")
cd $SCRIPTDIR/results/
python3 domainSize.py