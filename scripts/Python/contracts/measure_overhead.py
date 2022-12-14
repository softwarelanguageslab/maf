from lib.runner import *
from lib.experiments import *
import pandas as pd 
import os
import seaborn as sb
import matplotlib.pyplot as plt


def parse_stats(file): 
    with open(file) as f:
        lines = f.read()
        # split the lines in two
        sections = lines.split("---------------")
        output = []
        for section in sections[1:]: 
            lines = section.strip().split('\n')
            attribute_split = [ line.split(":") for line in lines ]
            pair = [ (split[0], int(split[1])) for split in attribute_split ]
            output.append(dict(pair))

        with_contracts = output[0]
        without_contracts = output[1]
        with_contracts["with_contracts"] = True 
        without_contracts["with_contracts"] = False

        return [
                with_contracts, without_contracts
        ]


if __name__ == "__main__":
    loader = ExperimentLoader.parse_argv()
    runner = ActorContractRunner(files=loader.items())

    output_path = runner.execute()
    output = []
    for file in os.listdir(output_path+"/stats"): 
        parsed = parse_stats(output_path+"/stats/"+file)
        for entry in parsed: 
            entry["filename"] = file

        output.extend(parsed)

    df = pd.DataFrame(output)
    print(df)
    print(f"Raw Output is available at {output_path}")
    sb.barplot(df, y = "# messages", x = "filename",  hue = "with_contracts")

plt.show()
