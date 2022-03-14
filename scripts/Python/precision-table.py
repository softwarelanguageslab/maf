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

def read_data(csv, merge_all):
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
    return frame[frame['config'] == 'CI-DI-WI']



full = read_data('type-curated-precision.csv', False)[['less', 'equal']].rename(columns={'equal': 'equal-full'})
noopt = read_data('type-curated-precision-noopt.csv', False)[['benchmark', 'equal', 'more']].rename(columns={'equal': 'equal-noopt'})
everything = pd.concat([noopt, full], axis=1)
print(everything.to_latex(index=False))

full = read_data('type-generated-precision.csv', True)[['less', 'equal']].rename(columns={'equal': 'equal-full'})
noopt = read_data('type-generated-precision-noopt.csv', True)[['benchmark', 'equal', 'more']].rename(columns={'equal': 'equal-noopt'})
everything = pd.concat([noopt, full], axis=1)
print(everything.to_latex(index=False))
