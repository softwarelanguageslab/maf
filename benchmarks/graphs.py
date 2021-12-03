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
            for time in metric["primaryMetric"]["rawData"][0]:
                output.append(dict(name = metric["benchmark"], time = time))
    return output

print("files in artifact: %s" % glob.glob("artifact/*.json"))

json_files = glob.glob("artifact/jmh-results-*-*.json")
print(json_files)
dates = [ datetime.date(*map(int, f.split('.')[0].split('-')[-3:])) for f in json_files ]
print(dates)
metrics = [ read_metrics(f) for f in json_files ]
for date, group in zip(dates, metrics):
    for metric in group:
        metric["date"] = date
        metric["name"] = metric["name"].replace("maf.cli.experiments.performance.PerformanceEvaluation", "")

df = pd.DataFrame(sum(metrics, []))
df["date"] = pd.to_datetime(df.date)
df = df.sort_values(by='name')

import matplotlib as mpl
mpl.use('Agg') # Needed to run without an X server, see here: https://stackoverflow.com/questions/4931376/generating-matplotlib-graphs-without-a-running-x-server
import seaborn as sb
import matplotlib.pyplot as plt

sb.set(rc={'figure.figsize':(11.7,8.27)})
sb.lineplot(x = "date", y = "time", hue = "name", data = df, marker = "o", ci = "sd")

moving_mean = pd.DataFrame()
for benchmark in set(df['name']):
    data = df[df['name'] == benchmark].groupby('date').mean().rolling(7).mean().assign(name=benchmark).reset_index()
    moving_mean = moving_mean.append(data)
moving_mean.insert(0, 'id', range(len(moving_mean)))
moving_mean = moving_mean.set_index('id')
moving_mean = moving_mean.sort_values(by='name')
sb.lineplot(x = "date", y = "time", hue = "name", data = moving_mean, ci = None, linestyle='--', legend=False)

plt.savefig("output.pdf")
