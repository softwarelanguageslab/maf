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
import maf.core.Position._
import maf.language.scheme._
import maf.modular.scheme.modf.SchemeModFComponent._
import maf.util.benchmarks.Timeout
import scala.concurrent.duration.*
import maf.language.scheme.LexicalRef
import maf.modular.scheme.modflocal.SchemeSemantics

     
object ConcreteSlicer:
    def runSlicer(program: SchemeExp): Unit = 
        val analysis = ConcreteSlicerDependencies.createAnalysis(program)
        analysis.analyzeWithTimeout(Timeout.start(30.seconds))
        val defs = analysis.finalDefs
        val deps = analysis.finalDeps
        printDefs(defs)
        deps.map((exp, dep) => printDeps(exp, dep))

    val hrLen = 20

    def printDeps(exp: SchemeExp, deps: Set[Address]): Unit = 
            println()
            println()
            println("+-+-+ " + exp + " +-+-+")
            println("_" * hrLen)
            println((" " * ((hrLen - 6)/2)) + "DEPS: ")
            deps.filter(_.printable).map(adr => println(adr))
            println()

    def printDefs(defs: Map[Address, Set[DefLoc]]): Unit =    
            print((" " * ((hrLen - 6)/2)) + "DEFS: ")
            println()
            defs.map((adr, locs) => 
                print(adr.toString + " ->")
                    locs.map(loc => 
                        print("  ")
                        print(loc.loc)
                        loc.index.map(idx => print("-" + idx)))
                    println())
            println()
            println("_" * hrLen)


            

