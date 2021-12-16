import pandas

[NAME, INIT, INIT_SD, REAN, REAN_SD, NOOPT, NOOPT_SD, CI, CI_SD, DI, DI_SD, WI, WI_SD, CIDI, CIDI_SD, CIWI, CIWI_SD, DIWI, DIWI_SD, CIDIWI, CIDIWI_SD] = range(21)
# Preprocessing: first run tail +1 performance\ generated.txt | grep -v ", ," > perf.csv
data = pandas.read_csv('perf.csv', header=None)

data_slower_than_100ms = data[data[INIT] >= 100]

# Filter the data to only keep these benchmarks that have 5 variants.
# This is less than optimal, but good enough
actual_data = data_slower_than_100ms
for name in data_slower_than_100ms[NAME]:
    # Get the base name of the benchmark (remove the -1.scm part)
    basename = '-'.join(name.split('-')[:-1])
    count = len([name2 for name2 in data_slower_than_100ms[NAME] if name2.startswith(basename)])
    if count < 5:
        print('Removing %s' % name)
        actual_data = actual_data[actual_data[NAME] != name]

print(actual_data)