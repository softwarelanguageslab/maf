package maf.util

import java.nio.file.{Files, Paths}

import sys.process.*

/**
 * A helper to run Python programs in the correct virtual environment.
 *
 * @param pwd
 *   the directory in which the Python script should be executed
 */
class PythonBridge(pwd: String = "./"):
    /** Prepares the virtual environment if none can be found */
    private def prepareEnvironment(): Boolean =
        // TODO: also run pip3 install -r requirements.txt to install dependencies
        if !Files.exists(Paths.get(s"$pwd/.venv")) then s"python3 -m venv $pwd/.venv".! == 0
        else true

    def runScript(program: String, arguments: String*): Boolean =
        s"cd $pwd && python3 $program ${arguments.mkString(" ")}".! == 0

    if prepareEnvironment() then println("Successfully prepared the Python virtual environment")
    else throw new Exception("Could not prepare the Python virtual environment")
