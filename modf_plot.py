import matplotlib
import matplotlib.pyplot as plt
import numpy as np
import csv

matplotlib.use("pdf")

def read_csv(csv_file):
    with open(csv_file) as fd:
        reader = csv.reader(fd,delimiter=",")
        _ = next(reader, None) # skip the headers
        return list(reader)

xticks = [1,2,4,8]

def find_data(data, name):
    for d in data:
        if d[0] == name:
            return d[1:]
    raise Exception('Cannot find data for ' + name)

def calculcate_speedups_and_error(benchmark, error_data, base_data):
    name = benchmark[0]
    base = int(find_data(base_data, name)[0])
    speedups = [base / int(benchmark[i]) for i in range(1, len(xticks)+1)]
    err = find_data(error_data, name)
    yerr = [speedups[i] - (base / (int(benchmark[i+1])+int(err[i]))) for i in range(0, len(xticks))]
    return speedups, yerr

MIN_TIME = 1000 # We don't want to plot benchmarks that take less than 1s in base modf
def plot_from_csv(base_csv_file, data_csv_file, error_csv_file, pdf_out_file, yticks):
    base_data = read_csv(base_csv_file)
    data = read_csv(data_csv_file)
    error_data = read_csv(error_csv_file)
    ax = plt.gca()
    ax.set_xscale('log', nonpositive='clip')
    for benchmark in data:
        if int(benchmark[1]) < MIN_TIME:
            continue
        speedups, yerr = calculcate_speedups_and_error(benchmark, error_data, base_data)
        (_, caps, _) = plt.errorbar(xticks,speedups, yerr=yerr, fmt='--o',label=benchmark[0], markersize=8, capsize=8) # , base=2
        for cap in caps:
            cap.set_markeredgewidth(1)
    plt.xlabel('Number of workers')
    plt.xticks(xticks,xticks)
    plt.ylabel('Speedup')
    plt.yticks(yticks,yticks)
    plt.grid()
    plt.legend()
    plt.savefig(pdf_out_file, pad_inches = 0, bbox_inches='tight')


metrics = {'call-depth': 1,
           'least-visited': 2,
           'most-visited': 3,
           'shallow-exp': 4,
           'deep-exp': 5,
           'most-deps': 6,
           'least-deps': 7,
           'bigger-env': 8,
           'smaller-env': 9}
paper_name = {
    'test/R5RS/WeiChenRompf2019/earley.sch': 'earley',
    'test/R5RS/WeiChenRompf2019/meta-circ.scm': 'meta-circ'
}
bar_width = 1.0 / (len(metrics) + 1) # 1 bar width of space between each bar group
def plot_metrics(base_csv_file, data_csv_file, error_csv_file, pdf_out_file, yticks):
    base_data = read_csv(base_csv_file)
    data = read_csv(data_csv_file)
    error_data = read_csv(error_csv_file)
    bars = {name: [] for (name, _) in metrics.items()}
    errors = {name: [] for (name, _) in metrics.items()}
    xpos = [[i for i in range(0, len(data))]]
    for i in range(1, len(metrics)+1):
        xpos = xpos + [[x + bar_width for x in xpos[i-1]]]
    for benchmark in data:
        base = int(find_data(base_data, benchmark[0])[0])
        if base < MIN_TIME:
            continue
        names = names + [paper_name[d[0]] for d in data]
        err = find_data(error_data, benchmark[0])
        for (metric, i) in metrics.items():
            bars[metric] = bars[metric] + [base / int(benchmark[i])]
            errors[metric] = errors[metric] + [base / (int(benchmark[i])+int(err[i-1]))]
    for (metric, i) in metrics.items():
        plt.bar(xpos[i-1], bars[metric], yerr=errors[metric], width=bar_width, edgecolor='white', label=metric)
    plt.xticks([i+0.5-bar_width for i in range(0, len(names))], names)
    plt.ylabel('Speedup')
    plt.xlabel('Benchmark')
    plt.grid()
    plt.legend()
    plt.savefig(pdf_out_file, pad_inches=0, bbox_inches='tight')
