#!/bin/bash

sbt "maf/runMain maf.cli.experiments.domain.DomainSize programs"

SCRIPTDIR=$(dirname "$0")
cd $SCRIPTDIR/results/
python3 programs.py