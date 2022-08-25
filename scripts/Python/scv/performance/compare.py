"""
Module to generate tables of the performance benchmarks
Used for generating the tables in Section 6A
"""

import glob
import pandas  as pd 
import numpy as np
import sys

# A statically defined set of groups, it is empty because each file will be its own group
GROUPS = []

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
    # Replace TIMEOUTS with NaN
    df["time"] = df["time"].replace("_", np.nan).replace("TIMEOUT", np.nan).astype(float)
    # Convert from nanoseconds to milliseconds
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
    # We need the (shortened) name of the benchmark
    df1 = df.assign(name = df["name"].map(lambda x: '/'.join(x.split("/")[-2:])))
    # All the units of time should be a float
    df1 = df1.assign(blames = df1["time"].astype(float))
    # Only select the relevant columns for our performance benchmarks
    df1 = df1[["name", "time", "configuration", "propagation phase (ms)", "collection phase (ms)"]]
    # The name and configuration will serve as the index of our dataframe
    df2 = df1.set_index(["name", "configuration"])
    # Collapse per group (none in this case see GROUPS)
    collapsed = df2.groupby(name_of_group)

    # Summarize our results.
    result = pd.DataFrame().assign(
        median_time = collapsed["time"].median(),
        max_time = collapsed["time"].max(),
        min_time = collapsed["time"].min(),
        avg_time = collapsed["time"].mean(),
        std_dev = collapsed["time"].std(),
        avg_prop = collapsed["propagation phase (ms)"].mean(),
        avg_coll = collapsed["collection phase (ms)"].mean()
    )

    # Compute the speedup
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



if len(sys.argv) != 2:
    # Try to automagically find the file in the results directory
    print("Automatically selecting result file to process")
    candidates = glob.glob("results/performance/*.csv")
    if len(candidates) > 1: 
        print("Too many candidates, reduces the number of files in results/performance to one!")
        exit(1)
    else:
        FILENAME = candidates[0]
else:
    # The filename is given by the user, use it
    FILENAME = sys.argv[1]

print("Processing", FILENAME)

df = load_dataframe(FILENAME)

df = df.assign(configuration = df["configuration"].replace({"scvFunctionSummariesTopSort": "(4)", "scvModf": "(1)", "scvRktFsR": "(2)", "scvFunctionSummaries": "(3)"}))
# Write it to a file
# df.to_csv("output-processed.csv")

summary = summarize_table(df) 

print("Ratio collection vs propagation phase")
compute_propagation_phase_vs_collection_phase_ratio(summary)

print("Latex execution time table")
print(to_latex(summary, "avg_time", "Average Time (ms)"))

# Write it to a file 
# summary.to_csv(FILENAME+"-summary.csv")

