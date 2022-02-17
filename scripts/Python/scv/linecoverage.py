"""
Produces graphs from the line coverage of the SCV file given in argsv of the program
"""

import pandas as pd 
import seaborn as sns
import matplotlib.pyplot as plt 
import sys

if len(sys.argv) != 2: 
    print("Not enough arguments")
    print("Usage: linecoverage.py CSV_FILE")
    sys.exit(1)

sns.set(rc={'figure.figsize':(11.7,8.27)})
sns.set_theme(style = "whitegrid")
filename = sys.argv[1]
df = pd.read_csv(filename)
df["name"] = df["name"].map(lambda v: v.split("/")[-1])
print(df)
ax = sns.barplot(x = "coverage", y = "name", data = df)
ax.axvline(x = df["coverage"].mean())
ax.set_xlabel("Line Coverage")
ax.set_ylabel("Benchmark")
# ax.tick_params(axis = "x", rotation = 90)
plt.savefig(filename + ".pdf")
