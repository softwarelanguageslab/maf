import modf_plot

yticks = [1,2,4]
modf_plot.plot_from_csv('data/modf-base-context-insensitive.csv', 'data/modf-context-insensitive.csv', 'data/modf-context-insensitive.csv-stddev', 'figures/modf-context-insensitive-speedups.pdf', yticks)
