package maf.cli.experiments.clients.concreteSlicing

import maf.modular.scheme.modf.BigStepModFSemanticsT
import maf.modular.ModAnalysis
import maf.core.Identifier
import maf.language.scheme.SchemeExp
import maf.core.Identity

import maf.modular.scheme.modf.SchemeModFSemanticsM
import maf.modular.scheme.modf.BigStepModFSemantics
import maf.modular.scheme.modf.BigStepModFSemanticsT
import maf.modular.scheme.modf.StandardSchemeModFComponents
import maf.modular.scheme.modf.SchemeModFNoSensitivity
import maf.modular.scheme.SchemeConstantPropagationDomain
import maf.modular.worklist.FIFOWorklistAlgorithm
import maf.language.symbolic.lattices.SymbolicSchemeConstantPropagationDomain
import maf.modular.scheme.modf.TEvalM

import maf.core._
import maf.core.Monad.MonadSyntaxOps
import maf.language.scheme._
import maf.modular.scheme.modf.SchemeModFComponent._
import maf.util.benchmarks.Timeout
import scala.concurrent.duration.*
import maf.language.scheme.LexicalRef
import maf.modular.scheme.modflocal.SchemeSemantics

import maf.modular.scheme.modflocal._

     
object ConcreteSlicer:

    // markExps returns a set of all expressions that impacted the slicing criterion
    def markExps(criterion: SchemeExp,
                 ctrlDeps: Map[SchemeExp, Option[SchemeExp]], 
                 dataDeps: Map[SchemeExp, Set[Address]], 
                 assignments: Map[Address, Set[SchemeExp]],
                 definitions: Map[Address, SchemeExp]): Set[SchemeExp] =
        def markExpsHelper(worklist: Set[SchemeExp], // the exps to do
                           finished: Set[SchemeExp], // the exps that have been done
                           addrs: Set[Address], // the addresses that are already kept
                           marks: Set[SchemeExp] // the marked expressions
                           ): Set[SchemeExp] = 
            if (worklist.isEmpty) then // nothing left to mark
                marks 
            else if (finished.contains(worklist.head)) then // this has been marked already
                markExpsHelper(worklist.tail, finished, addrs, marks)
            else // new exp to mark                
                // include the control dependency if present
                var depExps = ctrlDeps.getOrElse(worklist.head, None).toSet
                // gather data dependencies
                val depAddrs = dataDeps.getOrElse(worklist.head, Set.empty)
                // get the locations of the definitions and assignments of the data dependencies (if not already done)
                val defs = depAddrs.flatMap(addr => definitions.get(addr))
                val ass = depAddrs.flatMap(addr => assignments.getOrElse(addr, Set.empty))
                depExps = depExps ++ defs ++ ass

                // recursively slicing the subexpressions
                worklist.head match 
                    case l:SchemeLambdaExp => 
                        // if we are keeping a lambda, slice the lambda itself
                        val funcCriterion = l.body.last.asInstanceOf[SchemeExp]
                        val funMarks = markExps(funcCriterion, ctrlDeps, dataDeps, assignments, definitions)
                        depExps = (depExps - worklist.head) ++ funMarks + funcCriterion
                    case SchemeIf(prd, csq, alt, _) =>
                        // if we are keeping an if, keep the condition and slice the relevant branches
                        var ifMarks: Set[SchemeExp] = Set.empty
                        if(ctrlDeps.contains(csq)) then {
                            val csqCriterion = csq//csq.allSubexpressions.last.asInstanceOf[SchemeExp]
                            ifMarks = ifMarks ++ markExps(csqCriterion, ctrlDeps, dataDeps, assignments, definitions) + csqCriterion
                        }
                        if(ctrlDeps.contains(alt)) then {
                            val altCriterion = alt//alt.allSubexpressions.last.asInstanceOf[SchemeExp]
                            ifMarks = ifMarks ++ markExps(altCriterion, ctrlDeps, dataDeps, assignments, definitions) + altCriterion
                        }
                        depExps = (depExps - worklist.head) ++ (ifMarks + prd)
                    case SchemeBegin(exps, _) =>
                        // for a begin, keep only the last expression
                        // if assignments are in the begin, this is kept because of assignments and definitions
                        val beginCriterion = exps.last
                        val lastMarks = markExps(beginCriterion, ctrlDeps, dataDeps, assignments, definitions)
                        depExps = (depExps - worklist.head) ++ lastMarks + beginCriterion
                    // case SchemeSetLex(id, _, vexp, _) => 
                        // todo: do we really need this? 
                        // commented out because it creates an infinite loop
                    //     // the right hand side of a set! expressions should be sliced recursively
                    //     // (because assignments/definitions saves the entire set! expression)
                    //     val setMarks = markExps(vexp, ctrlDeps, dataDeps, assignments, definitions)
                    //     depExps = (depExps - worklist.head) ++ setMarks + vexp
                    case SchemeLet(_, body, _) => 
                        // for lets, the body should be sliced recursively
                        // the bindings are already sliced recursively because they are saved in the assignments/definitions
                        val letCriterion = body.last
                        val bodyMarks = markExps(letCriterion, ctrlDeps, dataDeps, assignments, definitions)
                        depExps = (depExps - worklist.head) ++ bodyMarks + letCriterion
                    case SchemeLetStar(_, body, _) => 
                        val letCriterion = body.last
                        val bodyMarks = markExps(letCriterion, ctrlDeps, dataDeps, assignments, definitions)
                        depExps = (depExps - worklist.head) ++ bodyMarks + letCriterion
                    case SchemeLetrec(_, body, _) => 
                        val letCriterion = body.last
                        val bodyMarks = markExps(letCriterion, ctrlDeps, dataDeps, assignments, definitions)
                        depExps = (depExps - worklist.head) ++ bodyMarks + letCriterion
                    case _ => depExps = depExps

                // add these new locs to the worklist
                val newWorklist = worklist.tail ++ depExps
                // continue iterating
                markExpsHelper(newWorklist, finished + worklist.head, addrs ++ depAddrs, marks ++ depExps)

        markExpsHelper(Set(criterion), Set.empty, Set.empty, Set.empty)

    def runSlicer(program: SchemeExp) = 
        //val analysis = ConcreteSlicerDependencies.createAnalysis(program)
        // println(program)
        // analysis.analyzeWithTimeout(Timeout.start(30.seconds))
        // val ass = analysis.finalAss
        // val defs = analysis.finalDefs
        // val deps = analysis.finalDeps
        // val ctrls = analysis.finalControlDeps

        def analysis = new SchemeModFConcreteDeps(program)
            with SchemeConstantPropagationDomain
            with SchemeModFLocalNoSensitivity
            with FIFOWorklistAlgorithm[SchemeExp]
        analysis.analyzeWithTimeout(Timeout.start(30.seconds))
        val ctrls = analysis.ctrlDeps 
        val ass: Map[Address, Set[SchemeExp]] = Map.empty
        val defs: Map[Address, SchemeExp] = Map.empty
        val deps: Map[SchemeExp, Set[Address]] = Map.empty
 
        // print results of the analysis
        printAss(ass)
        printDefs(defs)
        println()
        deps.map((e, d) => printDepsPerExp(e, d, ctrls.getOrElse(e, None)))

        // mark the expressions that influence the slicing criterion
        // TODO: dynamically pick the criterion
        // val criterion: SchemeExp = program.allSubexpressions.last.asInstanceOf[SchemeExp]
        // var marks = markExps(criterion, ctrls, deps, ass, defs)
        // println("_" * hrLen)
        // println()
        // println("program: " + program)
        // println("CRITERION: " + criterion)
        // println("MARKS: ")
        // println(marks)

        // marks.map(m => 
        //     var startCol = m.idn.pos.col
        //     if (m.toString.head.equals('(') && m.toString.length > 2) then {
        //         startCol = m.idn.pos.col - 1
        //     }
        //     (Position(m.idn.pos.line, startCol), Position(m.idn.pos.line, startCol + m.toString.length)))

    val hrLen = 40

    def printDepsPerExp(exp: SchemeExp, deps: Set[Address], ctrl: Option[SchemeExp]): Unit = 
            println()
            println()
            println("+-+-+ " + exp + " +-+-+")
            println("_" * hrLen)
            println("DATA DEPENDENCIES: ")
            deps.filter(_.printable).map(adr => println(adr))
            println()
            println("CONTROL DEPENDENCIES: ")
            println(ctrl)
            println()

    def printDefs(defs: Map[Address, SchemeExp]): Unit = 
        println()
        print("DEFINITIONS: ")
        println()
        defs.map((adr, loc) =>
            print(adr.toString + " -> ")
            print("  ")
            print(loc)
            println())
        println()
        println("_" * hrLen)

    def printAss(ass: Map[Address, Set[SchemeExp]]): Unit =   
            println() 
            print("ASSIGNMENTS: ")
            println()
            ass.map((adr, locs) => 
                print(adr.toString + " ->")
                    locs.map(loc => 
                        print("  ")
                        print(loc))
                        // loc.index.map(idx => print("-" + idx)))
                    println())
            println("_" * hrLen)

            

