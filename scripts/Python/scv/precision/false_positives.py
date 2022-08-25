#!/usr/bin/env ython

import pandas  as pd 
import seaborn as sb
import numpy as np
import matplotlib.pyplot as plt
import sys
import glob


if len(sys.argv) != 2:
    # Try to automagically find the file in the results directory
    print("Automatically selecting result file to process")
    candidates = glob.glob("results/precision/**.csv")
    if len(candidates) == 0: 
        print("No candidates found in results/precision")
        exit(1)
    if len(candidates) > 1: 
        print("Too many candidates, reduces the number of files in results/precision to one!")
        exit(1)
    else:
        FILENAME = candidates[0]
else:
    # The filename is given by the user, use it
    FILENAME = sys.argv[1]

df = pd.read_csv(FILENAME)

def preprocess(data):
    """
    This function splits the columns of the data frame such that we can fetch the metrics of each configuration
    """

    df = data.copy().set_index("benchmark")
    # the configuration name and the actual metric is seperated by an "_"
    df.columns = df.columns.str.split('_', expand=True).rename("configuration", level=0)
    # we need to stack them such that the configuration forms a seperate column
    df1 = df.stack(0).reset_index()

    df1 = df1.assign(configuration = df1["configuration"].replace({"scvModF-FunctionSummary-TopSort": "(3) and (4)", "scvModf": "(1)", "scvModF-rkt-fs-r": "(2)"}))
    df1 = df1.replace("ERROR", np.NaN)
    df1 = df1.replace("-", np.NaN)
    return df1
    #return df1.dropna()


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
    df1["# implicit contracts"] = df1["# implicit contracts"].replace("-", 0).astype(float)
    df1["# explicit contracts"] = df1["# explicit contracts"].replace("-", 0).astype(float)
    # Only keep the information about configuration, benchjmark and number of blames
    df1 = df1[["benchmark", "blames", "configuration", "# explicit contracts", "# implicit contracts"]]
    # Use benchmark and configuration as an index
    df1 = df1.set_index(["benchmark", "configuration"])
    # Collapse groups together
    collapsed = df1.groupby(name_of_group)
    # Compute some summarizing metrics
    df1 = pd.DataFrame().assign(
            # count = collapsed.count()["blames"], 
            median_blames = collapsed.median()["blames"], 
            # min_blames = collapsed.min()["blames"], 
            # max_blames = collapsed.max()["blames"],
            # median_contracts = collapsed.median()["# implicit contracts"] + collapsed.median()["# explicit contracts"]
    )
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

print("Precision table")
df1 = preprocess(df)

# Rename columns such that they appear like they appear in the paper
summary = summarize_into_table(df1)
# summary.to_csv(INPUT_FILE+"-summary.csv")
print(summary.to_latex(escape = False))
