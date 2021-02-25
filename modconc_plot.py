
import matplotlib
import matplotlib.pyplot as plt
import numpy as np
import csv

matplotlib.use("pdf")
workers = [1,2,4,8]
ROWS=4
COLS=4

def read_csv(csv_file):
    with open(csv_file) as fd:
        reader = csv.reader(fd,delimiter=",")
        _ = next(reader, None) # skip the headers
        result = []
        for row in reader:
            name = row[0]
            speedups = make_speedup_matrix(row)
            result.append((name, speedups))
        return result

def make_speedup_matrix(row):
    base = int(row[1])
    data = list(map(lambda x: base / int(x), row[2:]))
    return np.transpose(np.reshape(data, (len(workers),len(workers))))

def build_heatmaps(csv_file, out_file, w, h):
    data = read_csv(csv_file)
    fig, axs = plt.subplots(w,h)
    fig.set_figwidth(7)
    fig.set_figheight(7)
    plt.subplots_adjust(hspace=20, wspace=20)
    k = 0
    for i in range(0,w):
        for j in range(0,h):
            (benchmark_name, benchmark_data) = data[k]
            k = k + 1
            benchmark_axis = axs[i,j]
            benchmark_axis.set_title(benchmark_name, y=-0.2, fontsize=9)
            im = heatmap(benchmark_data, workers, workers, ax=benchmark_axis, cmap="RdYlGn", norm=MidPointLogNorm(vmin=0.2, midpoint=1, vmax=30))
            annotate_heatmap(im, valfmt="{x:.1f}x", textcolors=["black","black"],threshold=8,fontsize=7)
    fig.tight_layout()
    fig.savefig(out_file, pad_inches = 0.1, bbox_inches='tight')

##
## ADAPTED FROM https://matplotlib.org/3.1.1/gallery/images_contours_and_fields/image_annotated_heatmap.html
##

def heatmap(data, row_labels, col_labels, ax=None,
            cbar_kw={}, cbarlabel="", **kwargs):
    """
    Create a heatmap from a numpy array and two lists of labels.

    Parameters
    ----------
    data
        A 2D numpy array of shape (N, M).
    row_labels
        A list or array of length N with the labels for the rows.
    col_labels
        A list or array of length M with the labels for the columns.
    ax
        A `matplotlib.axes.Axes` instance to which the heatmap is plotted.  If
        not provided, use current axes or create a new one.  Optional.
    cbar_kw
        A dictionary with arguments to `matplotlib.Figure.colorbar`.  Optional.
    cbarlabel
        The label for the colorbar.  Optional.
    **kwargs
        All other arguments are forwarded to `imshow`.
    """

    if not ax:
        ax = plt.gca()

    # Plot the heatmap
    im = ax.imshow(data, **kwargs)
    im.axes.text(-0.8,-0.9,"n",fontsize=7)
    im.axes.text(-1.1,-0.5,"m",fontsize=7)
    im.axes.annotate("",
            xy=(-0.4, -0.4), 
            xytext=(-1.2, -1.2), 
            arrowprops=dict(arrowstyle="-", lw=0.2,
                      connectionstyle="arc3, rad=0"),
            )

    # Create colorbar (DON'T WANT THAT)
    #cbar = ax.figure.colorbar(im, ax=ax, **cbar_kw)
    #cbar.ax.set_ylabel(cbarlabel, rotation=-90, va="bottom")

    # We want to show all ticks...
    ax.set_xticks(np.arange(data.shape[1]))
    ax.set_yticks(np.arange(data.shape[0]))
    # ... and label them with the respective list entries.
    ax.set_xticklabels(col_labels, fontsize=7)
    ax.set_yticklabels(row_labels, fontsize=7)

    # Let the horizontal axes labeling appear on top.
    ax.tick_params(top=True, bottom=False,
                   labeltop=True, labelbottom=False)

    # Turn spines off and create white grid.
    for _, spine in ax.spines.items():
        spine.set_visible(False)

    ax.set_xticks(np.arange(data.shape[1]+1)-.5, minor=True)
    ax.set_yticks(np.arange(data.shape[0]+1)-.5, minor=True)
    ax.grid(which="minor", color="black", linestyle='-', linewidth=1)
    ax.tick_params(which="minor", bottom=False, left=False)

    return im


def annotate_heatmap(im, data=None, valfmt="{x:.2f}",
                     textcolors=["black", "white"],
                     threshold=None, **textkw):
    """
    A function to annotate a heatmap.

    Parameters
    ----------
    im
        The AxesImage to be labeled.
    data
        Data used to annotate.  If None, the image's data is used.  Optional.
    valfmt
        The format of the annotations inside the heatmap.  This should either
        use the string format method, e.g. "$ {x:.2f}", or be a
        `matplotlib.ticker.Formatter`.  Optional.
    textcolors
        A list or array of two color specifications.  The first is used for
        values below a threshold, the second for those above.  Optional.
    threshold
        Value in data units according to which the colors from textcolors are
        applied.  If None (the default) uses the middle of the colormap as
        separation.  Optional.
    **kwargs
        All other arguments are forwarded to each call to `text` used to create
        the text labels.
    """

    if not isinstance(data, (list, np.ndarray)):
        data = im.get_array()

    # Normalize the threshold to the images color range.
    if threshold is not None:
        threshold = im.norm(threshold)
    else:
        threshold = im.norm(data.max())/2.

    # Set default alignment to center, but allow it to be
    # overwritten by textkw.
    kw = dict(horizontalalignment="center",
              verticalalignment="center")
    kw.update(textkw)

    # Get the formatter in case a string is supplied
    if isinstance(valfmt, str):
        valfmt = matplotlib.ticker.StrMethodFormatter(valfmt)

    # Loop over the data and create a `Text` for each "pixel".
    # Change the text's color depending on the data.
    texts = []
    for i in range(data.shape[0]):
        for j in range(data.shape[1]):
            kw.update(color=textcolors[int(im.norm(data[i, j]) > threshold)])
            text = im.axes.text(j, i, valfmt(data[i, j], None), **kw)
            texts.append(text)

    return texts

##
## ADAPTED FROM https://stackoverflow.com/questions/48625475/python-shifted-logarithmic-colorbar-white-color-offset-to-center
##

class MidPointLogNorm(matplotlib.colors.LogNorm):
    def __init__(self, vmin=None, vmax=None, midpoint=None, clip=False):
        matplotlib.colors.LogNorm.__init__(self,vmin=vmin, vmax=vmax, clip=clip)
        self.midpoint=midpoint
    def __call__(self, value, clip=None):
        # I'm ignoring masked values and all kinds of edge cases to make a
        # simple example...
        x, y = [np.log(self.vmin), np.log(self.midpoint), np.log(self.vmax)], [0, 0.5, 1]
        return np.ma.masked_array(np.interp(np.log(value), x, y))

build_heatmaps('data/modconc.csv', 'figures/modconc-speedups.pdf', ROWS, COLS)

