import modf_plot

yticks = [1,2,4,8,16,32,64,128]
modf_plot.plot_from_csv('data/modf-base-context-sensitive-2CFA.csv', 'data/modf-context-sensitive-2CFA.csv', 'data/modf-context-sensitive-2CFA.csv-stddev', 'figures/modf-context-sensitive-speedups-2CFA.pdf', yticks)
