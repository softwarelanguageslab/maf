"""
Module to visualize performance of 
multiple configurations of scv
"""

import seaborn  as sb
import pandas  as pd 
import matplotlib.pyplot as plt 
from matplotlib.lines import Line2D
import sys

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

    return df 



FILENAME = sys.argv[1]
df = load_dataframe(FILENAME)

# Text rotation
plt.xticks(rotation = 45, ha = "right")
ax = sb.barplot(data = df, x = "name", y = "time", hue = "configuration")
ax.set_xlabel("Benchmark")
ax.set_ylabel("Time (ms)")
# Make sure that the rotated xticks fit in the figure
plt.tight_layout()
plt.show()
