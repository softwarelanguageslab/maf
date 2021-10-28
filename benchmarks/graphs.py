#!/usr/bin/env python
# coding: utf-8

import pandas as pd
import json
import glob


def read_metrics(filename):
    output = []
    with open(filename) as f:
        contents = f.read()
        metrics = json.loads(contents)
        for metric in metrics:
            output.append(dict(name = metric["benchmark"], time = metric["primaryMetric"]["score"]))
    return output


alldirs = glob.glob("artifact/*") ++ glob.glob("code/jvm/jmh-results-*.json")
dates = [ d.replace("artifact/", "").split("_")[0] for d in alldirs  ]
metrics = [ read_metrics(glob.glob(d+"/*.json")[0]) for d in alldirs ]
for date, group in zip(dates, metrics):
    for metric in group:
        metric["date"] = date
        metric["name"] = metric["name"].replace("maf.cli.experiments.performance.PerformanceEvaluation", "")

df = pd.DataFrame(sum(metrics, []))
df["date"] = pd.to_datetime(df.date)

import seaborn as sb
import matplotlib.pyplot as plt

sb.set(rc={'figure.figsize':(11.7,8.27)})
sb.lineplot(x = "date", y = "time", hue = "name", data = df)
plt.savefig("output.pdf")
