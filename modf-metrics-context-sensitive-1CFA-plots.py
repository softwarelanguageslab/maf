import modf_plot

yticks = [1,2,4]
modf_plot.plot_metrics('data/modf-base-context-sensitive.csv', 'data/modf-context-sensitive-metrics.csv', 'data/modf-context-sensitive-metrics.csv-stddev', 'figures/modf-context-insensitive-metrics.pdf', yticks)
