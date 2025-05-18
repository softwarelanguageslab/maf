import pandas as pd
import matplotlib.pyplot as plt

pd.set_option('display.max_columns', None)

df = pd.read_csv("output.csv")
df.set_index("Unnamed: 0", inplace=True)
df = df.drop("0", axis=1)

total = df.sum(axis=1).T
proportional = df.div(total, axis=0)

plt.rcParams['figure.figsize'] = [8, 5]

proportional.sum(axis=1)

rounded_p = proportional.applymap(lambda x: x*100).describe().drop(["count", "mean", "std"])

print(rounded_p)