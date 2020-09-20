package scalaam.modular.contracts

import scalaam.core.Position.Position
import scalaam.core.{Address, Environment}
import scalaam.language.contracts.ScLattice.Blame
import scalaam.language.contracts.{ScExp, ScIdentifier, ScLattice}
import scalaam.modular.{GlobalStore, ModAnalysis, ReturnAddr, ReturnValue}

trait ScModSemantics
    extends ModAnalysis[ScExp]
    with ScDomain
    with GlobalStore[ScExp]
    with ReturnValue[ScExp] {

  /**
    * This method can be overrided to implement a different strategy for allocating addresses for variables
    */
  type AllocationContext
  def allocVar(id: ScIdentifier, cmp: Component): ScVarAddr[AllocationContext]
  def allocGeneric(cmp: Component): ScGenericAddr[AllocationContext]

  type ComponentContext
  def allocCtx(
      clo: ScLattice.Clo[Addr],
      args: List[Value],
      call: Position,
      caller: Component
  ): ComponentContext

  def newComponent(component: Call[ComponentContext]): Component

  /**
    * The environment in which the analysis is executed
    */
  type Env = Environment[Address]

  /**
    * A base environment which can be defined by implementations of this trait
    */
  def baseEnv: Env

  /**
    * The components of this analysis are functions
    */
  type Component

  /**
    * For convience we define the `main` function as the initial component that must be analysed
    */
  def initialComponent: Component

  /**
    * Retrieves the expression from the given component
    * @param cmp the component to extract the expression from
    * @return an expression from the soft contract language
    */
  def expr(cmp: Component): ScExp

  def view(component: Component): ScComponent

  trait IntraScAnalysis extends IntraAnalysis with GlobalStoreIntra with ReturnResultIntra {

    /**
      * Compute the body of the component under analysis
      * @return the body of the component under analysis
      */
    def fnBody: ScExp = view(component) match {
      case ScMain             => program
      case Call(_, lambda, _) => lambda.body
    }

    /**
      * Compute the environment of the component under analysis
      * @return the body of the component under analysis
      */
    def fnEnv: Env = view(component) match {
      case ScMain          => baseEnv
      case Call(env, _, _) => env // TODO: extend environment with variable bindings
    }
  }

  override def intraAnalysis(component: Component): IntraScAnalysis

  def summary: ScAnalysisSummary[Value] = {
    var returnValues = Map[Any, Value]()
    var blames       = Map[Any, Set[Blame]]()

    store.foreach {
      case (ReturnAddr(cmp, _), value)    => returnValues = returnValues.updated(cmp, value)
      case (ExceptionAddr(cmp, _), value) => blames = blames.updated(cmp, lattice.getBlames(value))
      case _                              => ()
    }

    ScAnalysisSummary(returnValues, blames)
  }

  def getReturnValue(component: Component): Option[Value] = {
    summary.getReturnValue(component)
  }
}
