"""
Module to visualize performance of 
multiple configurations of scv
"""

import seaborn  as sb
import pandas  as pd 
import numpy as np
import matplotlib.pyplot as plt 
from matplotlib.lines import Line2D
import sys

# A statically defined set of groups
GROUPS = [
        #"softy",
        #"sergey", 
        #"mochi"
]

def load_dataframe(filename):
    """
    Load the data from the CSV file, and parse it into the correct format
    """
    
    # we set index to "name" so that it is not used in df.columns to split, and we keep the name
    df = pd.read_csv(filename).set_index("name")
    # columns are in format benchname_metric so transform such that each metric gets its own column
    df.columns = df.columns.str.split("_", expand = True).rename("configuration", level = 0)
    # From wide to long format
    df = df.stack(0).reset_index()
    # Make the name shorter
    df["name"] = df["name"].map(lambda x: "/".join(x.split("/")[-2:]))
    df["time"] = df["time"].replace("_", np.nan).replace("TIMEOUT", np.nan).astype(float)
    df["z3 (ns)"] = df["z3 (ns)"].replace("_", np.nan).astype(float)
    df["collection phase (ms)"] = df["collection phase (ms)"] / (1000*1000)
    df["propagation phase (ms)"] = df["propagation phase (ms)"] / (1000*1000)
    df["z3 (ns)"] = df["z3 (ns)"] / (1000*1000)
    return df 


def name_of_group(composite):
    name, configuration = composite
    for groupName in GROUPS:
        if name.startswith(groupName): 
            return (groupName, configuration)
    return (name, configuration)

def summarize_table(df):
    df1 = df.assign(name = df["name"].map(lambda x: '/'.join(x.split("/")[-2:])))
    df1 = df1.assign(blames = df1["time"].astype(float))
    df1 = df1[["name", "time", "configuration", "propagation phase (ms)", "collection phase (ms)"]]
    df2 = df1.set_index(["name", "configuration"])
    collapsed = df2.groupby(name_of_group)
    result = pd.DataFrame().assign(
        median_time = collapsed["time"].median(),
        max_time = collapsed["time"].max(),
        min_time = collapsed["time"].min(),
        avg_time = collapsed["time"].mean(),
        std_dev = collapsed["time"].std(),
        avg_prop = collapsed["propagation phase (ms)"].mean(),
        avg_coll = collapsed["collection phase (ms)"].mean()
    )

    result.index = pd.MultiIndex.from_tuples(result.index)
    unstacked = result.unstack()
    unstacked["speedup"] = unstacked["avg_time"]["(2)"] / unstacked["avg_time"]["(4)"] 
    
    return unstacked

def load_std(filename):
    """
    Load the standard deviation table into a DataFrame. Setting up the index on the name of the benchmark.

    :param filename the name of the file to load 
    """

    df = pd.read_csv(filename)
    df["name"] = df["name"].map(lambda x: "/".join(x.split("/")[-2:]))
    df = df.set_index("name")
    df.columns = df.columns.str.split("_", expand = True).rename("configuration", level = 0)
    unstacked = df.stack(0).unstack()
    selected = unstacked[["time"]].replace(["TIMEOUT", "_"], np.nan)
    print(selected)


def to_latex(df, time_column = "median_time", time_column_readable = "Median Time (ms)"): 
    """
    Convert the given DataFrame to a Latex format that can be used in the paper.

    :param df the dataframe as a result of summarize_table
    """

    selection = df[[time_column, "speedup"]]

    # Convert speedup to percentage
    selection = selection.assign(speedup = (selection["speedup"] - 1) * 100)
    # Add percentage sign to it, and if bigger than one put in bold
    bolded = selection["speedup"].apply(lambda x: "\\textbf{{{:10.2f}}}\%".format(x)) 
    formatted = selection["speedup"].apply(lambda x: "{:10.2f}\%".format(x))
    # Apply bolded on places where speedup > 0
    highlighted = formatted.where(selection["speedup"] < 0, bolded)

    # Underline the score that is the smallest in the median time   
    ## Compute the smallest value of each row (but drop the first configuration)
    minimum = selection[time_column].drop("(1)", axis=1).min(axis=1)
    ## Broadcast the smallest value such that the column is repeated for each configuration
    copied = selection.copy()
    copied[time_column].loc[:, :] = minimum.values[:, None]
    ## Compute the cell that has the minimum value
    mask = selection[time_column] != copied[time_column]
    ## Use the mask for conditional formatting
    highlight_median = selection[time_column].applymap(lambda x: "\\underline{{{}}}".format(x))
    selection[time_column] = selection[time_column].where(mask, highlight_median, axis = 1)

    # Compute averages as an extra row
    # avg =  selection.mean().rename("Average")
    # print(avg)
    # selection = selection.append(avg)

    # Rename for printing
    selection = selection.assign(speedup = highlighted)
    selection = selection.rename(columns = {time_column: time_column_readable, "speedup": "Speedup"})
    # Replace nan with -
    selection = selection.replace({np.nan: "-", "nan\%": "-"})

    return selection.to_latex(escape = False)

def compute_propagation_phase_vs_collection_phase_ratio(df):
    print("Collection Phase Slowdown Compared to the baseline")
    selection = df[["avg_prop", "avg_coll", "median_time"]]
    slowdown = (selection["avg_coll"]["(3)"] / selection["median_time"]["(1)"])
    print(slowdown)

    print("Propagation vs Collection Phase Ratio")
    ratio_coll = selection["avg_coll"]["(4)"] / selection["median_time"]["(4)"]
    ratio_prop = selection["avg_prop"]["(4)"] / selection["median_time"]["(4)"]
    print(pd.DataFrame().assign(ratio_coll = ratio_coll, ratio_prop = ratio_prop))
    print(ratio_coll.mean(), ratio_prop.mean())



FILENAME = sys.argv[1]
df = load_dataframe(FILENAME)

# Text rotation
plt.xticks(rotation = 45, ha = "right")
df = df.assign(configuration = df["configuration"].replace({"scvFunctionSummariesTopSort": "(4)", "scvModf": "(1)", "scvRktFsR": "(2)", "scvFunctionSummaries": "(3)"}))
# ax = sb.barplot(data = df, x = "name", y = "time", hue = "configuration", log = True)
# ax.set_xlabel("Benchmark")
# ax.set_ylabel("Time (ms)")
# Make sure that the rotated xticks fit in the figure
df.to_csv("output.csv")
summary = summarize_table(df) # .to_csv(FILENAME+"-summary.csv")

compute_propagation_phase_vs_collection_phase_ratio(summary)
exit(0)

load_std(FILENAME+"-stddev")
print(to_latex(summary, "avg_time", "Average Time (ms)"))
summary.to_csv(FILENAME+"-summary.csv")

# plt.tight_layout()
# plt.show()
