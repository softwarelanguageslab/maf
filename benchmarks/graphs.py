#!/usr/bin/env python
# coding: utf-8

import pandas as pd
import json
import glob
import os
import os.path
import datetime

def read_metrics(filename):
    output = []
    with open(filename) as f:
        contents = f.read()
        metrics = json.loads(contents)
        for metric in metrics:
            output.append(dict(name = metric["benchmark"], time = metric["primaryMetric"]["score"]))
    return output

json_files = glob.glob("artifact/*")
print(json_files)
dates = [ datetime.datetime.fromtimestamp(os.path.getmtime(f)) for f in json_files ]
print(dates)
metrics = [ read_metrics(f) for f in json_files ]
for date, group in zip(dates, metrics):
    for metric in group:
        metric["date"] = date
        metric["name"] = metric["name"].replace("maf.cli.experiments.performance.PerformanceEvaluation", "")

df = pd.DataFrame(sum(metrics, []))
df["date"] = pd.to_datetime(df.date)

import seaborn as sb
import matplotlib.pyplot as plt

sb.set(rc={'figure.figsize':(11.7,8.27)})
sb.lineplot(x = "date", y = "time", hue = "name", data = df, marker = "o")
plt.savefig("output.pdf")
