package scalaam.modular.contracts

import scalaam.core.Position.Position
import scalaam.core.{Address, Environment, Identity}
import scalaam.language.contracts.ScLattice.Blame
import scalaam.language.contracts.{ScExp, ScIdentifier, ScLambda, ScLattice}
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
  def allocGeneric(idn: Identity, cmp: Component): ScGenericAddr[AllocationContext]

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
    * The type of a call component creator
    */
  type CreateCallComponent = (Env, ScLambda, ComponentContext) => Call[ComponentContext]

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
      case ScMain => baseEnv
      case Call(env, lambda, _) =>
        env.extend(lambda.variables.map(v => (v.name, allocVar(v, component))))
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

  sealed trait SingleVerificationResult
  object SingleVerificationResult {
    def join(
        a: SingleVerificationResult,
        b: => SingleVerificationResult
    ): SingleVerificationResult = (a, b) match {
      case (Top, _)    => Top
      case (_, Top)    => Top
      case (Bottom, _) => b
      case (_, Bottom) => a
      case (_, _)      => Top
    }
  }
  case object Bottom           extends SingleVerificationResult
  case object UnverifiedSingle extends SingleVerificationResult
  case object VerifiedSingle   extends SingleVerificationResult
  case object Top              extends SingleVerificationResult
  var verificationResults: Map[Identity, Map[Identity, SingleVerificationResult]] =
    Map().withDefaultValue(Map().withDefaultValue(Bottom))

  def addSingleVerificationResult(
      monIdn: Identity,
      contractIdn: Identity,
      value: SingleVerificationResult
  ): Unit = {
    val monEntry      = verificationResults(monIdn)
    val contractEntry = monEntry(contractIdn)
    verificationResults += (monIdn -> (monEntry + (contractIdn ->
      SingleVerificationResult
        .join(contractEntry, value))))
  }

  def addVerified(monIdn: Identity, contractIdn: Identity): Unit = {
    addSingleVerificationResult(monIdn, contractIdn, VerifiedSingle)
  }

  def addUnverified(monIdn: Identity, contractIdn: Identity): Unit = {
    addSingleVerificationResult(monIdn, contractIdn, UnverifiedSingle)
  }

  def getVerificationResults(
      result: SingleVerificationResult,
      context: Option[Identity] = None
  ): List[Identity] = {
    val results = if (context.isEmpty) {
      verificationResults.values.flatten
    } else {
      verificationResults(context.get)
    }

    results
      .filter {
        case (_, `result`) => true
        case _             => false
      }
      .map {
        case (idn, _) => idn
      }
      .toList
  }
}
