from scipy import stats

import csv

def read_csv(csv_file):
    with open(csv_file) as fd:
        reader = csv.reader(fd,delimiter=",")
        header = next(reader, None) # skip the headers
        return (header, list(reader))

def find_data(data, name):
    for d in data:
        if d[0] == name:
            return d
    raise Exception('Cannot find data for ' + name)

metrics_columns = {} # map of metric -> column index
heuristics_columns = {} # map of metric -> column index

base_results = {} # map of benchmark name -> base time
heuristics_results = {} # map of benchmark name -> heuristic -> time
metrics_results = {} # map of benchmark name -> metric -> value
benchmarks = [] # list of benchmarks

def clear_data():
    global metrics_column
    global heuristics_columns
    global base_results
    global heuristics_results
    global metrics_results
    global benchmarks
    metrics_columns = {}
    heuristics_columns = {}
    base_results = {}
    heuristics_results = {}
    metrics_results = {}
    benchmarks = []

def load_data(base_csv_file, data_csv_file, metrics_csv_file):
    clear_data()
    (_, base_data) = read_csv(base_csv_file)
    (metrics_header, metrics_data) = read_csv(metrics_csv_file)
    for i in range(1, len(metrics_header)): # Skip the first one (column for benchmark name)
        metrics_columns[metrics_header[i]] = i
    (heuristics_header, data) = read_csv(data_csv_file)
    for i in range(1, len(heuristics_header)):
        heuristics_columns[heuristics_header[i]] = i

    for benchmark in data:
        name = benchmark[0]

        base_results[name] = float(find_data(base_data, name)[1])

        metric_res = find_data(metrics_data, name)
        metrics_results[name] = {}
        for (metric, col) in metrics_columns.items():
            metrics_results[name][metric] = float(metric_res[col])
        heuristics_results[name] = {}
        for (heuristic, col) in heuristics_columns.items():
            heuristics_results[name][heuristic] = float(benchmark[col])
        benchmarks.append(name)

def find_correlation(variant, heuristic, metric):
    heuristic_data = []
    metric_data = []
    for benchmark in benchmarks:
        heuristic_data.append(heuristics_results[benchmark]['random'] / heuristics_results[benchmark][heuristic])
        metric_data.append(metrics_results[benchmark][metric])
    assert len(heuristic_data) == len(metric_data)
    rho, p = stats.spearmanr(metric_data, heuristic_data)
    if rho > 0 and p < 0.05:
        print(f'Correlation between heuristic {heuristic} and metric {metric} ({variant}): rho = {rho}, p = {p}')

if __name__ == '__main__':
    for metric_variant in ['mean', 'max', 'stddev']:
        load_data('data/modf-base-context-sensitive.csv', 'data/modf-context-sensitive-metrics.csv', 'data/modf-context-sensitive-metrics-' + metric_variant + '.csv')
        for metric in metrics_columns.keys():
            for heuristic in heuristics_columns.keys():
                find_correlation(metric_variant, heuristic, metric)
