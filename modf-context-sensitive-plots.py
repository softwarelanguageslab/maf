import modf_plot

yticks = [1,2,4,8,16,32,64,128]
modf_plot.plot_from_csv('data/modf-base-context-sensitive.csv', 'data/modf-context-sensitive.csv', 'data/modf-context-sensitive.csv-stddev', 'figures/modf-context-sensitive-speedups.pdf', yticks)
