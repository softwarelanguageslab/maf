#!/usr/bin/python3
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import numpy as np

def parse_filename(fullname):
    """Parses a full file path into its name, e.g., foo/bar/baz.scm -> baz"""
    return fullname.split('/')[-1].split('.')[0]

def parse_data(csv):
    """Parses the data from a CSV file"""
    data = pd.read_csv(csv)
    for column in data.columns:
        if column == 'benchmark':
            data[column] = data[column].apply(parse_filename)
    return data

def plot_line_rel(csv, ax, title):
    data = parse_data(csv)
    for benchmark in data['benchmark']:
        row = data[data['benchmark'] == benchmark]
        plot_data = [
#            row['ms (rean)'].values[0] / row['ms (init)'].values[0],
            row['ms (NoOpt)'].values[0] / row['ms (rean)'].values[0],
            row['ms (CI)'].values[0] / row['ms (rean)'].values[0],
            row['ms (DI)'].values[0] / row['ms (rean)'].values[0],
            row['ms (CI-DI)'].values[0] / row['ms (rean)'].values[0],
            row['ms (WI)'].values[0] / row['ms (rean)'].values[0],
            row['ms (DI-WI)'].values[0] / row['ms (rean)'].values[0],
            row['ms (CI-WI)'].values[0] / row['ms (rean)'].values[0],
            row['ms (CI-DI-WI)'].values[0] / row['ms (rean)'].values[0],
        ]
        ax.plot(plot_data, label=benchmark)
    ax.set_xlabel(title)
    ax.set_xticklabels(['none', 'NoOpt', 'CI', 'DI', 'CI-DI', 'WI', 'DI-WI', 'CI-WI', 'CI-DI-WI'])
    ax.set_ylabel('Relative time to reanalysis')
    ax.set_yscale('log')

def plot_line(csv, ax, title):
    data = parse_data(csv)
    for benchmark in data['benchmark']:
        row = data[data['benchmark'] == benchmark]
        plot_data = [
            row['ms (rean)'].values[0],
            row['ms (NoOpt)'].values[0],
            row['ms (CI)'].values[0],
            row['ms (DI)'].values[0],
            row['ms (CI-DI)'].values[0],
            row['ms (WI)'].values[0],
            row['ms (DI-WI)'].values[0],
            row['ms (CI-WI)'].values[0],
            row['ms (CI-DI-WI)'].values[0],
        ]
        ax.plot(plot_data, label=benchmark)
    ax.set_xlabel(title)
    ax.set_yscale('log')
    ax.set_xticklabels(['none', 'rean', 'NoOpt', 'CI', 'DI', 'CI-DI', 'WI', 'DI-WI', 'CI-WI', 'CI-DI-WI'])
    ax.set_ylabel('Time (ms)')

def plot_box(csv, ax, title, filtering = lambda x: x, log = True):
    data = filtering(parse_data(csv))
    if len(data) == 0:
        return
    sns.boxplot(ax=ax, data=[
        np.divide(data['ms (NoOpt)'], data['ms (rean)']),
        np.divide(data['ms (CI)'], data['ms (rean)']),
        np.divide(data['ms (DI)'], data['ms (rean)']),
        np.divide(data['ms (CI-DI)'], data['ms (rean)']),
        np.divide(data['ms (WI)'], data['ms (rean)']),
        np.divide(data['ms (DI-WI)'], data['ms (rean)']),
        np.divide(data['ms (CI-WI)'], data['ms (rean)']),
        np.divide(data['ms (CI-DI-WI)'], data['ms (rean)']),
    ])
    ax.set_xlabel(title + ' ({} programs)'.format(len(data)))
    ax.set_xticklabels(['NoOpt', 'CI', 'DI', 'CI-DI', 'WI', 'DI-WI', 'CI-WI', 'CI-DI-WI'])
    ax.set_ylabel('Time relative to full reanalysis')
    if log:
        ax.set_yscale('log')
    ax.hlines(1, -0.5, 7.5, color='red')
    print('slowest:')
    data['slowdown'] = np.divide(data['ms (CI)'], data['ms (rean)'])
    print(data[data['slowdown'] > 1][['benchmark', 'slowdown']])


fig, ax = plt.subplots(4)
fig.set_size_inches(16, 16)
fig.set_dpi(200)
plot_box('type-curated-performance.csv', ax[0], 'Curated', log = False)
plot_box('type-generated-performance.csv', ax[1], 'Generated, initial analysis <1s, full reanalysis <1s', lambda x: x[x['ms (rean)'] < 1000][x['ms (init)'] < 1000])
#plot_box('type-generated-performance.csv', ax[2], 'Generated, initial analysis <1s, full reanalysis >=1s', lambda x: x[x['ms (rean)'] >= 1000][x['ms (init)'] < 1000])
plot_box('type-generated-performance.csv', ax[2], 'Generated, initial analysis >=1s, full reanalysis <1s', lambda x: x[x['ms (rean)'] < 1000][x['ms (init)'] >= 1000])
plot_box('type-generated-performance.csv', ax[3], 'Generated, initial analysis >=1s, full reanalysis >=1s', lambda x: x[x['ms (rean)'] >= 1000][x['ms (init)'] >= 1000])

plt.savefig('type-performance.pdf')

fig, ax = plt.subplots(4)
fig.set_size_inches(16, 16)
fig.set_dpi(200)
plot_box('cp-curated-performance.csv', ax[0], 'Curated', log = False)
plot_box('cp-generated-performance.csv', ax[1], 'Generated, initial analysis <1s, full reanalysis <1s', lambda x: x[x['ms (rean)'] < 1000][x['ms (init)'] < 1000])
#plot_box('cp-generated-performance.csv', ax[2], 'Generated, initial analysis <1s, full reanalysis >=1s', lambda x: x[x['ms (rean)'] >= 1000][x['ms (init)'] < 1000])
plot_box('cp-generated-performance.csv', ax[2], 'Generated, initial analysis >=1s, full reanalysis <1s', lambda x: x[x['ms (rean)'] < 1000][x['ms (init)'] >= 1000])
plot_box('cp-generated-performance.csv', ax[3], 'Generated, initial analysis >=1s, full reanalysis >=1s', lambda x: x[x['ms (rean)'] >= 1000][x['ms (init)'] >= 1000])
plt.savefig('cp-performance.pdf')
