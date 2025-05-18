import pandas as pd
import json
import subprocess
import matplotlib.pyplot as plt

pd.set_option('display.max_columns', None)

programs = pd.read_csv("programs.csv", header=None).set_index(0)
programs_by_category = programs.stack() \
                      .reset_index(level=1) \
                      .rename_axis('source') \
                      .drop("level_1", axis=1) \
                      .reset_index().rename(columns={0: "filename"})

def count(filename):
    out  = subprocess.check_output(['tokei', filename, '--output', 'json'])
    data = json.loads(out)
    return data['Total']['code']

programs_by_category["count"] = programs_by_category["filename"].map(lambda name: count("../" + name))

stats = programs_by_category.groupby("source").describe().T.reset_index().drop('level_0', axis=1).set_index('level_1').rename_axis('').T
stats = stats[["count", "mean", "min", "50%", "max"]]

print(stats)