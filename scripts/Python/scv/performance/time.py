"""
Module to visualize the results of the SCV benchmarks. 

This file visualizes the time it takes for the analysis to complete.
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

def transform_time_data(df): 
    # TODO: make this inline
    df["name"] = df["name"].map(lambda x: "/".join(x.split("/")[-2:]))
    df["scvModf (z3 (ns))"] = df["scvModf (z3 (ns))"] / (1000*1000)
    df["scvModf (z3 interpreter (ns))"] = df["scvModf (z3 interpreter (ns))"] / (1000*1000)
    return df

def plot_total_against_z3(df): 
    pbp = PerformanceBarPlot(data = df, x = "name", y = "scvModf", label = "Total time (ms)")
    pbp.overlay(x = "name", y = "scvModf (z3 (ns))", color = "red", label = "Time PC sat (ms)")
    pbp.overlay(x = "name", y = "scvModf (z3 interpreter (ns))", color = "blue", label = "Time in Z3 Interpreter")
    pbp.build(x_label = "Benchmark", y_label = "Time (ms)")
    plt.savefig("plot_total_against_z3.pdf")


def plot_z3_cache_against_checks(df): 
    pbp = PerformanceBarPlot(data = df, x = "name", y = "scvModf (# z3 executions)", label = "Total # Solves")
    pbp.overlay(x = "name", y = "scvModf (# cache hits)", color = "red", label = "# Cache Hits")
    pbp.build(x_label = "Benchmark", y_label = "# Solves")
    plt.savefig("plot_z3_cache_hits.pdf")

INPUT_FILENAME = sys.argv[1]
df = read_data(INPUT_FILENAME)
df = transform_time_data(df)
plot_total_against_z3(df)
plot_z3_cache_against_checks(df)
