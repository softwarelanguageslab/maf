package maf.cli.experiments.incremental

import scala.annotation.tailrec

case class IncArgs(
    performance: Boolean = false,
    precision: Boolean = false,
    curated: Boolean = false,
    generated: Boolean = false,
    typeLattice: Boolean = false,
    cpLattice: Boolean = false,
    warmUp: Int = 3,
    repetitions: Int = 15,
    count: Option[Int] = None)

object RunIncrementalEvaluation:

    def parseArgs(args: Array[String]): IncArgs =
        @tailrec
        def processArgs(args: List[String], options: IncArgs): IncArgs = args match {
          case "--performance" :: tail                                => processArgs(tail, options.copy(performance = true))
          case "--precision" :: tail                                  => processArgs(tail, options.copy(precision = true))
          case "--curated" :: tail                                    => processArgs(tail, options.copy(curated = true))
          case "--generated" :: tail                                  => processArgs(tail, options.copy(generated = true))
          case "--type" :: tail                                       => processArgs(tail, options.copy(typeLattice = true))
          case "--cp" :: tail                                         => processArgs(tail, options.copy(cpLattice = true))
          case "--warmup" :: n :: tail if n.forall(Character.isDigit) => processArgs(tail, options.copy(warmUp = n.toInt))
          case "--repet" :: n :: tail if n.forall(Character.isDigit)  => processArgs(tail, options.copy(repetitions = n.toInt))
          case "--count" :: n :: tail if n.forall(Character.isDigit)  => processArgs(tail, options.copy(count = Some(n.toInt)))
          case Nil                                                    => options
          case o =>
            System.err.nn.println(s"Unknown options: $o")
            sys.exit(1)
        }
        if args.length == 0 then
            println("""Arguments:
            |   --performance  run the performance experiments
            |   --precision    run the precision experiments
            |   --curated      run experiments that use the curated benchmarking suite
            |   --generated    run experiments that use the generated benchmarking suite
            |   --type         run experiments that use the type lattice
            |   --cp           run experiments that use the cp lattice
            |   --warmup n     use n warmup runs for the performance experiments
            |   --repet n      use n measured runs for each performance experiment
            |   --count n      use only n benchmarks from the benchmarking suite
            | Any combination of arguments can be used.
            | To run experiments, you will at least need to provide a type of experiment (performance/precision),
            | a benchmarking suite (curated/generated), a lattice (type/cp).
            | Example arguments: --precision --curated --type""".stripMargin)
            sys.exit(0)
        else processArgs(args.toList, IncArgs())
    end parseArgs

    def main(args: Array[String]): Unit =
        val arguments = parseArgs(args)
        if arguments.precision then
            println("Measuring precision.")
            try IncrementalSchemeModXPrecision.main(arguments)
            catch
                case t: Throwable =>
                  System.err.nn.println(s"Measuring precision failed: ${t.getMessage}.")
        end if
        /*
        try IncrementalSchemeModXProperties.main(args)
        catch
            case t: Throwable =>
              System.err.nn.println(s"Obtaining properties failed: ${t.getMessage}.")
         */
        if arguments.performance then
            println("Measuring performance.")
            try IncrementalSchemeModXPerformance.main(arguments) // Run this last to have the other results sooner.
            catch
                case t: Throwable =>
                  System.err.nn.println(s"Measuring performance failed: ${t.getMessage}.")
        end if
        println("Done.")
    end main
