package maf.cli.runnables

import maf.bench.scheme.SchemeBenchmarkPrograms
import maf.util.Reader
import maf.util.Writer
import java.nio.file.Files

/**
 * Generates random inputs matching the contracts in the provide/out expressions.
 *
 * The random inputs are generated using the input/racket-input-generator/ Racket language extension, which replaces all provide contract/out
 * expressions with contract-random-generate expressions.
 *
 * The output of each such invocation is a list of inputs for each function exported in the provide expression
 */
object ScvInputGenerator:
    def benchmarks: Set[String] = SchemeBenchmarkPrograms.scvNguyenBenchmarks

    /**
     * Read the program from the given path into a string
     *
     * @param path
     *   the path to the program to read (must be a file reachable from the current working directory)
     * @return
     *   the program as a string
     */
    private def readProgram(path: String): String =
      Reader.loadFile(path)

    /**
     * Replace the first line with <code>#lang reader random-scv</code> if that line starts with a <code>#lang</code>
     *
     * @param program
     *   the entire program as a stringa
     * @return
     *   a program that is modified at the #lang line
     */
    private def replaceLang(program: String): String =
        val rest = program.split('\n').tail.toList
        val totalProgram = "#lang reader random-scv" :: rest
        totalProgram.mkString("\n")

    /**
     * *
     *
     * Runs the program using the Racket interpreter (must be available on the path)
     *
     * @param program
     *   the program that is fed to the stdin of the Racket interpreter
     * @return
     *   the output of the Racket program
     */
    private def run(program: String, benchmark: String): String =
        // We can only provide input to Racket through a file or through the REPL. But since we don't want to use the REPL we will use a file.
        val tmpFile = Files.createTempFile(null, ".rkt")
        val openFile = Writer.open(tmpFile.toString)
        // Write to the tmpFile
        openFile.write(program)
        openFile.close
        // Run the file using Racket
        import scala.sys.process._
        try s"racket ${tmpFile}".!!
        catch
            case e: Exception =>
              println(s"$benchmark $e")
              "(fail)"

    /** Write the given contents to a file */
    private def writeToFile(contents: String, path: String = ""): Unit =
        val writer = Writer.open(s"input/generated/${path.replace("/", "_")}.scm")
        writer.write(contents)
        writer.close

    def main(args: Array[String]): Unit =
        val programs = benchmarks
        // collected all the generated inputs into a string
        val outputs = programs.map(readProgram andThen replaceLang).zip(programs).map(run)
        // and write them to a file
        outputs.zip(programs).foreach(writeToFile(_, _))
