#!/usr/bin/env python
# coding: utf-8
import json
import glob
import datetime
import pandas as pd
import matplotlib as mpl
mpl.use('Agg') # Needed to run without an X server, see here: https://stackoverflow.com/questions/4931376/generating-matplotlib-graphs-without-a-running-x-server
import seaborn as sb
import matplotlib.pyplot as plt
import matplotlib.ticker as tkr

def read_metrics(filename):
    output = []
    with open(filename) as f:
        contents = f.read()
        metrics = json.loads(contents)
        for metric in metrics:
            output.append(dict(name = metric["benchmark"], time = metric["primaryMetric"]["score"], error = metric["primaryMetric"]["scoreError"]))
    return output

print("files in artifact: %s" % glob.glob("artifact/*.json"))

json_files = glob.glob("artifact/jmh-results-*-*.json")
dates = [ datetime.date(*map(int, f.split('.')[0].split('-')[-3:])) for f in json_files ]
metrics = [ read_metrics(f) for f in json_files ]
for date, group in zip(dates, metrics):
    for metric in group:
        metric["date"] = date
        metric["name"] = metric["name"].replace("maf.cli.experiments.performance.PerformanceEvaluation", "")

df = pd.DataFrame(sum(metrics, []))
df["date"] = pd.to_datetime(df.date)
df = df.sort_values(by='name')

sb.set(rc={'figure.figsize':(11.7,8.27)})

def ytick_formatter(x, pos):
    return '{}s'.format(int(x / 1000))
yfmt = tkr.FuncFormatter(ytick_formatter)
ax = plt.gca()
ax.yaxis.set_major_formatter(yfmt)

for benchmark in set(df['name']):
    color = next(ax._get_lines.prop_cycler)['color']
    data = df[df['name'] == benchmark]
    # Plot the individual points with an error bar
    plt.errorbar(data['date'], data['time'], yerr=data['error'], marker = "o", linestyle="", label=benchmark, color=color)
    # Compute the 7 day moving mean
    moving_mean = data.groupby('date').mean().rolling(7).mean().assign(name=benchmark).reset_index()
    plt.plot(moving_mean['date'], moving_mean['time'], linestyle='--', color=color)

plt.legend()
plt.savefig("output.pdf")
