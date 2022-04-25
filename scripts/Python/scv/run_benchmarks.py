#!/usr/bin/env python3

import subprocess

benchmark_classes = [
        "maf.cli.experiments.aam.ScvPerformanceComparison",
        "maf.cli.experiments.scv.CountFalsePositives"
]

visualisation_scripts = [
        None,
        None
]

def run_benchmarks(): 
    for cls in benchmark_classes:
        print(f"running {cls}")
        subprocess.run(f"sbt maf/runMain {cls}")
        print(f"finished {cls}")

def visualize_results(): 
    # TODO
    pass


print("running the benchmarks")
run_benchmarks()
print("visualizing results")
visualize_results()
print("done")
