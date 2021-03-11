package maf.test

import maf.language.contracts.ScExp
import maf.language.contracts.ScLattice.Blame
import maf.modular.contracts.{ScSMTSolverJVM, ScSmtSolver}
import maf.test.contracts.{ScTestGlobalStore, ScTestLocalStore}

trait ScTestsJVM extends ScTests with ScAnalysisTests {
  trait ScTestAnalysisJVM extends ScTestAnalysis {
    override def newSmtSolver(program: PC): ScSMTSolverJVM =
      new ScSMTSolverJVM(program, primitivesMap)
  }

  def newAnalysis(program: ScExp): ScTestAnalysisJVM
}

trait ScTestsJVMLocalStore extends ScTestsJVM with ScTestLocalStore {
  class ScTestsJVMLocalStore(prg: ScExp)
      extends ScTestAnalysis(prg)
      with ScTestAnalysisJVM
      with ScTestAnalysisLocalStore

  def newAnalysis(program: ScExp): ScTestsJVMLocalStore = {
    new ScTestsJVMLocalStore(program)
  }
}

trait ScTestsJVMGlobalStore extends ScTestsJVM with ScTestGlobalStore {
  class ScTestsJVMGlobalStore(prg: ScExp)
      extends ScTestAnalysis(prg)
      with ScTestAnalysisJVM
      with ScTestAnalysisGlobalStore

  def newAnalysis(program: ScExp): ScTestsJVMGlobalStore = {
    new ScTestsJVMGlobalStore(program)
  }
}
