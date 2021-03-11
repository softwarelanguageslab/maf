package maf.modular.contracts

import maf.language.contracts.ScExp
import maf.language.contracts.ScNil
import maf.language.contracts.ScIdentifier

/**
  * A function summary keeps track of symbolic
  * information of the results of a function call.
  *
  * For example, the summary of
  * (define (or a b)
  *   (or a b))
  *
  * is (or x0 x1) with a map mapping variables to symbolic
  * versions: (a -> x0, b -> x1)
  */
trait FunctionSummary extends ScModSemantics {
  case class FunctionSummary(
      variables: Map[ScIdentifier, ScExp],
      returnValues: List[ScExp],
      constraints: Map[Addr, ScExp]
  )

  var summaries: Map[Component, List[FunctionSummary]] = Map()

  override def intraAnalysis(component: Component): IntraFunctionAnalysis

  trait IntraFunctionAnalysis extends IntraScAnalysis {

    /**
      * A map that maps syntactic variable to a symbolic one.
      * This is specific to each function invocation.
      */
    private var variables: Map[ScIdentifier, ScExp] = Map()

    /**
      * A map from variables to constraints on these variables
      */
    private var constraints: Map[Addr, ScExp] = Map()

    /**
      * Adds an additional constraint to the given variable
      *
      * @param variable the variable to add the constraint to
      * @param constraint the constraint to add
      */
    def constrain(variable: Addr, constraint: ScExp): Unit = {
      val value = constraints.getOrElse(variable, ScNil())
      constraints += variable -> value.and(constraint)
    }

    def mapVariable(variable: ScIdentifier, symbolicVariable: ScExp): Unit = {
      variables += variable -> symbolicVariable
    }

    /**
      * Soundly merges an existing summary with the current one
      */
    def finishSummary(returnValues: List[ScExp]): Unit = {
      val value = summaries.get(component).getOrElse(List())
      summaries += component -> (FunctionSummary(
        variables,
        returnValues,
        constraints
      ) :: value)
    }

  }

}
