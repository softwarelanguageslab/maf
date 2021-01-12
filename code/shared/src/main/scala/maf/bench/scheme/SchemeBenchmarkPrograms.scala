package maf.bench.scheme

import java.io.File

import maf.util.datastructures.SmartUnion

import scala.util.Random
object SchemeBenchmarkPrograms {

  def files(dir: File): Array[File] = {
    val lst = dir.listFiles()
    if (lst == null) Array()
    else lst
  }

  // Just include an entire folder of programs, except some that are explicitly specified. The directory name should not end with a "/".
  def fromFolderR(directory: String)(exclude: String*): Set[String] = {
    def recur(dir: File): Array[File] = {
      val here = files(dir)
      val (dr, noDr) = here.partition(_.isDirectory)
      noDr ++ dr.flatMap(recur)
    }
    val root = new File(directory)
    val base = root.getAbsolutePath.length - directory.length
    recur(root).map(_.getAbsolutePath.substring(base)).toSet -- exclude.map(file => s"$directory/$file")
  }

  def fromFolder(directory: String)(exclude: String*): Set[String] = {
    val root = new File(directory)
    val base = root.getAbsolutePath.length - directory.length
    files(root).filter(!_.isDirectory).map(_.getAbsolutePath.substring(base)).toSet -- exclude.map(file => s"$directory/$file")
  }

  // Only include certain listed programs.
  def toFolder(directory: String)(include: String*): Set[String] = include.map(file => s"$directory/$file").toSet

  // SEQUENTIAL ////////////////////////////////////////////////////////////////////////////////////////////////////////

  lazy val ad: Set[String] = fromFolder("test/R5RS/ad")(
    "bfirst.scm", // Unbound identifier: create-graph
    "bst.scm", // Tail of empty list BUT THERE IS NO CODE TO RUN? TODO Look at this.
    "btree.scm", // Lacks a body.
    "stspaceCODE.scm" // Tail of empty list BUT THERE IS NO CODE TO RUN? TODO Look at this.
  )
  lazy val gabriel: Set[String] = fromFolder("test/R5RS/gabriel")()
  lazy val gambit: Set[String] = fromFolder("test/R5RS/gambit")(
    "compiler.scm", // Does not parse
    "ctak.scm", // Needs call-with-current-continuation.
    "fibc.scm", // Needs call-cc.
    "puzzle.scm", // Needs call-with-current-continuation.
    "scheme.scm", // Error in program BUT CAN BE ANALYSED.
    "slatex.scm", // Needs string primitive
    "trav1.scm", // Needs append in the abstract interpreter (not as a preluded primitive)
  )
  lazy val icp1: Set[String] = fromFolder("test/R5RS/icp")(
    "icp_1c_ambeval.scm", // Undefined variable read.
    "icp_4_qeval.scm" // Needs define-syntax and delay.
  )
  lazy val rosetta: Set[String] = fromFolder("test/R5RS/rosetta")()
  lazy val scp1: Set[String] = fromFolder("test/R5RS/scp1")(
    "circus.scm" // Vararg append not supported by concrete interpreter.
  )
  lazy val scp1_compressed: Set[String] = fromFolder("test/R5RS/scp1-compressed")()
  lazy val scp1_singleFile: Set[String] = Set("test/R5RS/scp1-compressed/all.scm")
  lazy val sigscheme: Set[String] = fromFolder("test/R5RS/sigscheme")()
  lazy val theLittleSchemer: Set[String] = fromFolder("test/R5RS/WeiChenRompf2019/the-little-schemer")(
    "ch4.scm", // No main code (only definitions).
    "ch5.scm", // No main code (only definitions).
    "ch7.scm", // No main code (only definitions).
    "ch9.scm", // Unbound identifier: will-stop?
    "ch10.scm" // Tail of empty list BUT THERE IS NO CODE TO RUN? TODO Look at this.
  )
  lazy val toplas98: Set[String] = fromFolder("test/R5RS/WeiChenRompf2019/toplas98")(
    "graphs.scm", // Uses read
    "handle.scm", // Uses defmacro (not standard r5rs).
    "maze.scm", // Uses read
    "nbody.scm", // Apply cannot handle this apparently.
    "nbody-processed.scm", // Apply cannot handle this apparently.
    "nucleic.sch", // Does not parse
    "nucleic2.sch", // Uses macros (define-syntax).
    "splay.scm" // Uses () instead of '(), and #(1 2 3) syntax for vectors
  )
  lazy val WCR2019: Set[String] = fromFolder("test/R5RS/WeiChenRompf2019")(
    ".DS_Store",
    "earley.sch", // Uses read.
    "mbrotZ.sch", // Uses read.
    "solovay-strassen.scm" // Program seems erroneous.
  )
  lazy val other: Set[String] = fromFolder("test/R5RS")(
    ".DS_Store",
    "pico.scm", // Used def-macro, no main body + need to incorporate pico.ini file.
    "quasiquoting.scm", // Uses unquote-splicing.
    "Streams.scm", // Uses define-macro.
    "callcc.scm" // call/cc not yet support in concrete interpreter
  )
  lazy val WeiChenRompf2019: Set[String] = SmartUnion.sunionList(List(theLittleSchemer, toplas98, WCR2019))
  lazy val sequentialBenchmarks: Set[String] =
    SmartUnion.sunionList(List(ad, gabriel, gambit, icp1, rosetta, scp1, sigscheme, WeiChenRompf2019, other))

  def selectRandomSeq(n: Int): Set[String] = Random.shuffle(sequentialBenchmarks).take(n)

  // CONCURRENT ////////////////////////////////////////////////////////////////////////////////////////////////////////

  lazy val savina: Set[String] = fromFolder("test/concurrentScheme/actors/savina")()
  lazy val soter: Set[String] = fromFolder("test/concurrentScheme/actors/soter")()
  lazy val actors: Set[String] = savina ++ soter ++ fromFolder("test/concurrentScheme/actors")()
  lazy val futures: Set[String] = fromFolder("test/concurrentScheme/futures")()
  lazy val futuresVariations: Set[String] = fromFolder("test/concurrentScheme/futures/variations")()
  lazy val threads: Set[String] = fromFolder("test/concurrentScheme/threads")(
    "abp.scm", // Unbound reference: display-recorded.
    "lastzero2.scm" // Uses let*, but should use something like letrec*?
  )
  lazy val threadsVariations: Set[String] = fromFolder("test/concurrentScheme/threads/variations")()
  lazy val concurrentBenchmarks: Set[String] =
    SmartUnion.sunionList(List(actors, futures, futuresVariations, savina, soter, threads, threadsVariations))

  def selectRandomPar(n: Int): Set[String] = Random.shuffle(concurrentBenchmarks).take(n)

  // ALL ///////////////////////////////////////////////////////////////////////////////////////////////////////////////

  lazy val allBenchmarks: Set[String] = SmartUnion.sunion(concurrentBenchmarks, sequentialBenchmarks)
}
