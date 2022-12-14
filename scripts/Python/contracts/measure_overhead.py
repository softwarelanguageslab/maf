from lib.runner import *
from lib.experiments import *


if __name__ == "__main__":
    loader = ExperimentLoader.parse_argv()
    runner = ActorContractRunner(files=loader.items())

    output_path = runner.execute()
    print(f"Output is available at {output_path}")
