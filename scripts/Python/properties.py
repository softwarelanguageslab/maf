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

def plot_box(csv, ax, prop):
    data = parse_data(csv)
    configs = ['NoOpt', 'CI', 'DI', 'CI-DI', 'WI', 'DI-WI', 'CI-WI', 'CI-DI-WI']
    sns.boxplot(ax=ax, data=[data['{} ({})'.format(prop, column)] for column in configs])
    ax.set_xticklabels(configs)
    ax.set_ylabel(prop)
    ax.set_yscale('log')

def plot_affected(csv, ax):
    data = parse_data(csv)
    sns.boxplot(ax=ax, data = data['Dir. imp. Cmp.'] / data['#Components (init)'])
    ax.set_ylabel('Directly impacted components')

fig, ax = plt.subplots(5)
fig.set_size_inches(17, 17)
fig.set_dpi(200)
plot_box('type-properties.csv', ax[0], '#Analyses')
plot_box('type-properties.csv', ax[1], '#Components')
plot_box('type-properties.csv', ax[2], '#Dependencies')
plot_box('type-properties.csv', ax[3], '|Store|')
plot_affected('type-properties.csv', ax[4])
plt.savefig('type-properties.pdf')

