"""
Module to visualize the results of the scv precision comparison
"""

import seaborn  as sb
import pandas  as pd 
import matplotlib.pyplot as plt 
from matplotlib.lines import Line2D
import sys

class PerformanceBarPlot: 
    def __init__(self, data, x, y, label, color = "grey"): 
        # clear the plot 
        plt.clf()
        # Keep track of the original dataframe 
        self.df = data
        # Text rotation
        plt.xticks(rotation = 45, ha = "right")
        # base bars
        self.ax = sb.barplot(x = x, y = y, data = data, color = color)
        # keep track of the labels and their colors 
        self.labels = [label]
        self.colors = [color]
        
    def overlay(self, x, y, color, label, data = None): 
        df = data if data else self.df

        sb.barplot(x = x, y = y, data = df, color = color, ax = self.ax)
        self.labels.append(label)
        self.colors.append(color)

    def build(self, x_label, y_label): 
        self.ax.set_xlabel(x_label)
        self.ax.set_ylabel(y_label)
        lines = [Line2D([0], [0], color=color, lw=4) for color in self.colors]
        self.ax.legend(lines, self.labels)
        plt.tight_layout()

def read_data(filename): 
    return pd.read_csv(filename)

def preprocess_data(df):
    df["benchmark"] = df["benchmark"].map(lambda x: "/".join(x.split("/")[-2:]))
    return df

def plot_precision(df):
    # Here we make the addresses that are more precise in the concrete interpreter grey, so that it looks that the exactly 
    # precise addresses are at the top of each bar in the plot
    pbp = PerformanceBarPlot(data = df, x = "benchmark", y = "scv-modf-total", label = "# Precise Addresses", color = "lime") # total amount of addresses
    pbp.overlay(x = "benchmark",  y = "scv-modf", label = "# Total Addresses", color = "grey") # more precise addresses 
    pbp.build(x_label = "Benchmark", y_label = "# Addresses")
    plt.savefig("precision_scv.pdf")

FILENAME = sys.argv[1]
df = read_data(FILENAME)
df = preprocess_data(df)
plot_precision(df)

