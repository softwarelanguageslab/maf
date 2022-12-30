import os
import sys 
import json

class ExperimentLoader: 
    def __init__(self, files):
        self.files = files

    def items(self): 
        """
        Returns the list of files to load
        """
        print("Files are ", self.files)
        return self.files

    @staticmethod 
    def parse_argv():
        """
        Retrieves an EXPERIMENT.json from the command line
        """
        if len(sys.argv) < 2: 
            print("Insufficient amount of parameters given.")
            print("Usage: ")
            print(f"{sys.argv[0]} EXPERIMENT.json [SUBSECTION]")
            sys.exit()

        experiment = sys.argv[1]
        if len(sys.argv) == 3: 
            subsection = sys.argv[2]
        else: 
            subsection = None
        
        if not(os.path.exists(experiment)): 
            print(f"Experiment at {experiment} not found")
            sys.exit()
    
        with open(experiment) as f:
            filenames = json.load(f)
            if subsection:
                if subsection in filenames:
                    filenames = filenames[subsection]
                else: 
                    print(f"Invalid subsection given {subsection}")

                sys.exit()
            else: 
                filenames = [ item for sublist in filenames.values() for item in sublist ]

        # prefix the file names with the path of the experiments file
        dirname = os.path.dirname(experiment)
        filenames = [ dirname+"/"+filename for filename in filenames ]

        return ExperimentLoader(filenames)

