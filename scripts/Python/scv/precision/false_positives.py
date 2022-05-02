#!/usr/bin/env python

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
    # we need to stack them such that the configuration forms a seperate column
    df1 = df.stack(0).reset_index()
    df1 = df1.replace("ERROR", np.NaN)
    df1 = df1.replace("-", np.NaN)
    return df1.dropna()


def visualize_false_positive_count(df):
    """
    Visualizes the false positives for each benchmark as a barplot

    :param df the dataframe to use as a data source
    """

    df1 = df.assign(benchmark = df["benchmark"].map(lambda x: '/'.join(x.split("/")[-2:])))
    df1 = df1.assign(blames = df1["blames"].astype(int))

    ax = sb.barplot(data = df1, x = "benchmark", y = "blames", hue = "configuration") 
    ax.set_ylabel("# False Positives")
    plt.xticks(rotation = 45, ha = "right")
    plt.tight_layout()
    plt.show()

df1 = preprocess(df)
visualize_false_positive_count(df1)

