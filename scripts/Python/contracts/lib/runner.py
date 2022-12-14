
import os
import subprocess
import tempfile
import shutil


class Runner: 
    """
    A command-line runner that expects its output is a particular subdirectory.
    """

    def __init__(self, command, out_path, final_path = None):
        """ 
        :param command the command to run
        :param out_path the output directory of the executable
        :param final_path the path to move the result directory to
        """
        self.out_path = out_path
        self.command  = command
        self.final_path = final_path

    
    def execute(self): 
        """
        Execute the command, returns the path where the results are stored.
        """

        temporary_location = None
        if os.path.exists(self.out_path): 
            # the directory already exists, let's move it to a different location 
            temp_dir = tempfile.mkdtemp()
            temporary_location = temp_dir+"/"+self.out_path
            shutil.move(self.out_path, temporary_location)

        # run the command
        subprocess.run(self.command, stdout=subprocess.DEVNULL)

        if temporary_location:  
            # create a location for our results to be placed in
            new_path = self.final_path if self.final_path else tempfile.mkdtemp() 
            shutil.move(self.out_path, new_path)

            # if we moved the original output path we should move it back
            shutil.move(temporary_location, self.out_path)
        else: 
            new_path = self.out_path

        return new_path


    @staticmethod
    def run(command, out_path): 
        return Runner(command, out_path)


class ActorContractRunner(Runner):
    """
    A runner for actor contracts
    """

    def __init__(self, files, final_path=None, base = "acontracts/"):
        """
        :param base the base path where to find the benchmarking utilities for the contracts
        """
        super().__init__([base+"bench_all.sh"] + files, "stats", final_path)
