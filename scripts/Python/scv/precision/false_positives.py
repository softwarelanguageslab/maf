#!/usr/bin/env ython

import pandas  as pd 
import seaborn as sb
import numpy as np
import matplotlib.pyplot as plt
import sys

INPUT_FILE =  sys.argv[1]
df = pd.read_csv(INPUT_FILE)

def preprocess(data):
    """
    This function splits the columns of the data frame such that we can fetch the metrics of each configuration
    """

    df = data.copy().set_index("benchmark")
    # the configuration name and the actual metric is seperated by an "_"
    df.columns = df.columns.str.split('_', expand=True).rename("configuration", level=0)
    print(df)
    # we need to stack them such that the configuration forms a seperate column
    df1 = df.stack(0).reset_index()
    df1 = df1.replace("ERROR", np.NaN)
    df1 = df1.replace("-", np.NaN)
    return df1
    #return df1.dropna()


def visualize_false_positive_count(df):
    """
    Visualizes the false positives for each benchmark as a barplot

    :param df the dataframe to use as a data source
    """

    df1 = df.assign(benchmark = df["benchmark"].map(lambda x: '/'.join(x.split("/")[-2:])))
    df1 = df1.assign(blames = df1["blames"].astype(float))

    ax = sb.barplot(data = df1, x = "benchmark", y = "blames", hue = "configuration") 
    ax.set_ylabel("# False Positives")
    plt.xticks(rotation = 45, ha = "right")
    plt.tight_layout()
    plt.show()

def name_of_group(composite):
    name, configuration = composite
    for groupName in GROUPS:
        if name.startswith(groupName): 
            return (groupName, configuration)
    return (name, configuration)

def summarize_into_table(df):
    """
    Summarize the false positives into a table 

    :param df the dataframe to use as a source 
    :param groups of benchmarks that must be groups together
    """
    # Convert the name to something shorter
    df1 = df.assign(benchmark = df["benchmark"].map(lambda x: '/'.join(x.split("/")[-2:])))
    # Convert the number of blames to a float
    df1 = df1.assign(blames = df1["blames"].astype(float))
    # Convert the number of contract checks to a float
    df1["# check expressions"] = df1["# check expressions"].replace("-", 0).astype(float)
    # Only keep the information about configuration, benchjmark and number of blames
    df1 = df1[["benchmark", "blames", "configuration", "# check expressions"]]
    print(df1)
    # Use benchmark and configuration as an index
    df1 = df1.set_index(["benchmark", "configuration"])
    # Collapse groups together
    collapsed = df1.groupby(name_of_group)
    print(collapsed.mean())
    # Compute some summarizing metrics
    df1 = pd.DataFrame().assign(count = collapsed.count(), median_blames = collapsed.median()["blames"], min = collapsed.min()["blames"], max = collapsed.max()["blames"])
    # Convert the tuple to a multi index
    df1.index = pd.MultiIndex.from_tuples(df1.index)
    # Unstack again
    return df1.unstack()

# A statically defined set of groups
GROUPS = [
        "softy",
        "sergey", 
        "mochi"
]

df1 = preprocess(df)
summary = summarize_into_table(df1)
summary.to_csv(INPUT_FILE+"-summary.csv")
# visualize_false_positive_count(df1)
