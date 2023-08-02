package maf.cli.experiments.incremental

import maf.modular.incremental.IncrementalConfiguration
import maf.util.ColouredFormatting.*

import scala.annotation.tailrec

case class IncArgs(
    performance: Boolean = false,
    precision: Boolean = false,
    properties: Boolean = false,
    curated: Boolean = false,
    generated: Boolean = false,
    typeLattice: Boolean = false,
    cpLattice: Boolean = false,
    warmUp: Int = 3,
    repetitions: Int = 15,
    count: Option[Int] = None,
    stopOnError: Boolean = false,
    files: Set[String] = Set(),
    timeout: Int = -1,
    config: Option[IncrementalConfiguration] = None)

object RunIncrementalEvaluation:

    def parseArgs(args: Array[String]): IncArgs =
        @tailrec
        def processArgs(args: List[String], options: IncArgs): IncArgs = args match {
            case "--performance" :: tail                                    => processArgs(tail, options.copy(performance = true))
            case "--precision" :: tail                                      => processArgs(tail, options.copy(precision = true))
            case "--properties" :: tail                                     => processArgs(tail, options.copy(properties = true))
            case "--curated" :: tail                                        => processArgs(tail, options.copy(curated = true))
            case "--generated" :: tail                                      => processArgs(tail, options.copy(generated = true))
            case "--type" :: tail                                           => processArgs(tail, options.copy(typeLattice = true))
            case "--cp" :: tail                                             => processArgs(tail, options.copy(cpLattice = true))
            case "--warmup" :: n :: tail if n.forall(Character.isDigit)     => processArgs(tail, options.copy(warmUp = n.toInt))
            case "--repet" :: n :: tail if n.forall(Character.isDigit)      => processArgs(tail, options.copy(repetitions = n.toInt))
            case "--count" :: n :: tail if n.forall(Character.isDigit)      => processArgs(tail, options.copy(count = Some(n.toInt)))
            case "--stop" :: tail                                           => processArgs(tail, options.copy(stopOnError = true))
            case "--file" :: f :: tail                                      => processArgs(tail, options.copy(files = options.files + f))
            case "--timeout" :: m :: tail if m.forall(Character.isDigit)    => processArgs(tail, options.copy(timeout = m.toInt))
            case "--config" :: c :: tail =>
                IncrementalConfiguration.fromString(c) match {
                    case None    => System.err.nn.println(s"Unknown configuration: $c"); sys.exit(2)
                    case Some(c) => processArgs(tail, options.copy(config = Some(c)))
                }
            case Nil => options
            case o   => System.err.nn.println(s"Unknown options: $o"); sys.exit(1)
        }
        if args.length == 0 then
            println("""Arguments:
            |   --performance  run the performance experiments
            |   --precision    run the precision experiments
            |   --properties   run the properties experiments
            |   --curated      run experiments that use the curated benchmarking suite
            |   --generated    run experiments that use the generated benchmarking suite
            |   --file         run the experiments on a certain file (this argument can be specified multiple times)
            |   --type         run experiments that use the type lattice
            |   --cp           run experiments that use the cp lattice
            |   --warmup n     use n warmup runs for the performance experiments
            |   --repet n      use n measured runs for each performance experiment
            |   --count n      use only n benchmarks from the benchmarking suite
            |   --stop         use to stop on errors (errors will be thrown and not be caught)
            |   --timeout      specify the timeout to be used for each run of the (incremental) analysis in minutes
            |   --config c     specify the configuration to be used
            | Any combination of arguments can be used.
            | To run experiments, you will at least need to provide a type of experiment (performance/precision/properties),
            | a benchmarking suite (curated/generated/a specific file), and a lattice (type/cp).
            | Example arguments: --precision --curated --type""".stripMargin)
            sys.exit(0)
        else processArgs(args.toList, IncArgs())
    end parseArgs

    def main(args: Array[String]): Unit =
        val arguments = parseArgs(args)

        if arguments.precision then
            println(markHeader("Measuring precision."))
            IncrementalSchemeModXPrecision.main(arguments)

        if arguments.properties then
            println(markHeader("Measuring properties."))
            IncrementalSchemeModXProperties.main(arguments)

        // Run this last to have the other results sooner.
        if arguments.performance then
            println(markHeader("Measuring performance."))
            IncrementalSchemeModXPerformance.main(arguments)

        println(markOK("Done."))
    end main
