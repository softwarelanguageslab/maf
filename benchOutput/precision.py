#!/usr/bin/python3
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import numpy as np

def parse_precision(prec):
    """Parses precision information from a string like '11 (100.00%)' into a tuple (11, 100)"""
    [count, percent] = prec.split(' ')
    count = int(count)
    percent = float(percent.split('(')[1].split('%)')[0])
    return (count, percent)

def parse_precision_count(prec):
    """Parses precision information, keeping only the raw count"""
    return parse_precision(prec)[0]

def parse_precision_pct(prec):
    """Parses precision information, keeping only the percentage"""
    return parse_precision(prec)[1]

def parse_filename(fullname):
    """Parses a full file path into its name, e.g., foo/bar/baz.scm -> baz"""
    return fullname.split('/')[-1].split('.')[0]

def parse_data(csv):
    """Parses the data from a CSV file"""
    data = pd.read_csv(csv)
    for column in data.columns:
        if column == 'benchmark':
            data[column] = data[column].apply(parse_filename)
            continue
        else:
            data[column] = data[column].apply(parse_precision_count)
    return data

def plot_line(csv, ax, title):
    data = parse_data(csv)
    for benchmark in data['benchmark']:
        row = data[data['benchmark'] == benchmark]
        plot_data = [
            row['Less precise (NoOpt)'].values[0],
            row['Less precise (CI)'].values[0],
            row['Less precise (DI)'].values[0],
            row['Less precise (CI-DI)'].values[0],
            row['Less precise (WI)'].values[0],
            row['Less precise (DI-WI)'].values[0],
            row['Less precise (CI-WI)'].values[0],
            row['Less precise (CI-DI-WI)'].values[0]
        ]
        ax.plot(plot_data, label=benchmark)
    ax.set_xlabel(title)
    ax.set_xticklabels(['none', 'NoOpt', 'CI', 'DI', 'CI-DI', 'WI', 'DI-WI', 'CI-WI', 'CI-DI-WI'])
    ax.set_ylabel('% less precise')

def plot_box(csv, ax, title):
    data = parse_data(csv)
    sns.boxplot(ax=ax, data=[
        data['Less precise (NoOpt)'],
        data['Less precise (CI)'],
        data['Less precise (DI)'],
        data['Less precise (CI-DI)'],
        data['Less precise (WI)'],
        data['Less precise (DI-WI)'],
        data['Less precise (CI-WI)'],
        data['Less precise (CI-DI-WI)'],
    ])
    ax.set_xlabel(title)
    ax.set_xticklabels(['NoOpt', 'CI', 'DI', 'CI-DI', 'WI', 'DI-WI', 'CI-WI', 'CI-DI-WI'])
    ax.set_ylabel('% less precise')

def plot_bars(csv, ax, title, merge_all):
    data = parse_data(csv)
    benchmark_data = {'benchmark': [], 'config': [], 'less': [], 'equal': [], 'more': [], 'lessplusequal': [], 'lessplusequalplusmore': [], 'equalplusmore': []}
    for benchmark in data['benchmark']:
        row = data[data['benchmark'] == benchmark]
        for config in ['NoOpt', 'CI', 'DI', 'CI-DI', 'WI', 'DI-WI', 'CI-WI', 'CI-DI-WI']:
            equal = row['Equal ({})'.format(config)].values[0]
            less = row['Less precise ({})'.format(config)].values[0]
            more = row['More precise ({})'.format(config)].values[0]
            benchmark_data['benchmark'].append(benchmark)
            benchmark_data['config'].append(config)
            benchmark_data['equal'].append(equal)
            benchmark_data['less'].append(less)
            benchmark_data['more'].append(more)
            benchmark_data['lessplusequal'].append(less + equal)
            benchmark_data['lessplusequalplusmore'].append(less + equal + more)
            benchmark_data['equalplusmore'].append(equal + more)
    frame = pd.DataFrame(benchmark_data)
    if merge_all:
        new_frame = {'benchmark': [], 'config': [], 'less': [], 'equal': [], 'more': [], 'lessplusequal': [], 'lessplusequalplusmore': [], 'equalplusmore': []}
        for config in ['NoOpt', 'CI', 'DI', 'CI-DI', 'WI', 'DI-WI', 'CI-WI', 'CI-DI-WI']:
            new_frame['benchmark'].append('All')
            new_frame['config'].append(config)
            for metric in ['equal', 'less', 'more', 'lessplusequal', 'equalplusmore', 'lessplusequalplusmore']:
                new_frame[metric].append(frame[frame['config'] == config][metric].sum())
        frame = pd.DataFrame(new_frame)
    print(frame)
    sns.barplot(ax=ax, data=frame, x='benchmark', hue='config', y='lessplusequalplusmore', color='darkred')
    sns.barplot(ax=ax, data=frame, x='benchmark', hue='config', y='equalplusmore', color='darkgreen')
    sns.barplot(ax=ax, data=frame, x='benchmark', hue='config', y='equal', color='darkgray')

    ax.set_xlabel(title)
    ax.set_xticklabels(ax.get_xticklabels(), rotation=45, ha='right')
    ax.set_ylabel('Precision per address')
    ax.set_yscale('log')
    ax.get_legend().remove()

def plot(csv, path, title, group):
    fig, ax = plt.subplots(1)
    fig.set_size_inches(17, 13)
    fig.set_dpi(200)
    plot_bars(csv, ax, title, group)
    plt.savefig(path)

plot('type-curated-precision-noopt.csv', 'type-curated-precision-noopt.pdf', 'Curated', False)
plot('type-generated-precision-noopt.csv', 'type-generated-precision-noopt.pdf', 'Generated', True)
plot('type-curated-precision.csv', 'type-curated-precision.pdf', 'Curated', False)
plot('type-generated-precision.csv', 'type-generated-precision.pdf', 'Generated', True)

plot('cp-curated-precision-noopt.csv', 'cp-curated-precision-noopt.pdf', 'Curated', False)
plot('cp-generated-precision-noopt.csv', 'cp-generated-precision-noopt.pdf', 'Generated', True)
plot('cp-curated-precision.csv', 'cp-curated-precision.pdf', 'Curated', False)
plot('cp-generated-precision.csv', 'cp-generated-precision.pdf', 'Generated', True)


# TODO split <100 and >=100
