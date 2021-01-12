package maf.modular.contracts

import maf.core.Position.Position
import maf.core.{Address, Environment, Identity}
import maf.language.contracts.ScLattice.Blame
import maf.language.contracts.{ScExp, ScIdentifier, ScLambda, ScLattice, ScParam}
import maf.modular.{
  DestructiveStore,
  GlobalStore,
  LocalStore,
  LocalStoreMap,
  ModAnalysis,
  ReturnAddr,
  ReturnValue
}
import maf.core.Lattice
import maf.lattice.interfaces.BoolLattice

object ScModSemantics {
  var r = 0
  def genSym: String = {
    r += 1
    s"x$r"
  }

  def freshIdent: ScExp = {
    ScIdentifier(genSym, Identity.none)
  }
}

trait ScModSemantics
    extends ModAnalysis[ScExp]
    with ScDomain
    with GlobalStore[ScExp]
    with ReturnValue[ScExp]
    with DestructiveStore[ScExp]
    with LocalStore[ScExp] {

  type ComponentContext
  type AllocationContext
  type VarAllocationContext
  def allocVar(id: ScIdentifier, cmp: ComponentContext): ScVarAddr[VarAllocationContext]
  def allocGeneric(idn: Identity, cmp: Component): ScGenericAddr[AllocationContext]

  /**
    * Returns the context of a component
    */
  def context(component: Component): ComponentContext

  /**
    * Allocates a new context
    */
  def allocCtx(
      clo: ScLattice.Clo[Addr],
      args: List[Value],
      call: Position,
      caller: Component
  ): ComponentContext

  /**
    * Creates a new component based on the given concrete component
    */
  def newComponent(component: Call[ComponentContext]): Component

  def viewStore(component: Component): Store = view(component) match {
    case c: CallWithStore[ComponentContext, Addr, Value] => c.store.v
    case ScMain | Call(_, _, _) =>
      if (GLOBAL_STORE_ENABLED) Map()
      else
        store.view.filterKeys {
          case _: ReturnAddr[_] | _: ExceptionAddr[_] => false
          case _                                      => true
        }.toMap
  }

  def instrument(component: Component, data: LocalStoreMap[Addr, Value]): Component =
    view(component) match {
      // we can only pass the store to calls of other functions
      case c: Call[ComponentContext] => newComponent(CallWithStore(c, data))

      case _ => component
    }

  def deinstrument(component: Component): Component = component match {
    case c: CallWithStore[ComponentContext, Addr, Value] => newComponent(c.call)
    case _                                               => component
  }

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
  type Component <: Serializable

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

  /**
    * Set to true if the analysis must use a global store, for all its variables.
    * Set to false if the global store should only be used for return values and for parameter passing
    * to top-level functions.
    */
  val GLOBAL_STORE_ENABLED: Boolean

  def view(component: Component): ScComponent

  implicit def toScIdentifier(p: ScParam): ScIdentifier =
    ScIdentifier(p.name, p.idn)

  trait IntraScAnalysis
      extends IntraAnalysis
      with GlobalStoreIntra
      with ReturnResultIntra
      with DestructiveStoreIntra
      with LocalStoreIntra { intra =>

    /**
      * Remove a blame of a certain component from the store
      */
    def removeBlame(component: Component, idn: Identity): Unit = {
      intra.store = intra.store
        .map {
          case (ex @ ExceptionAddr(`component`, _), value) =>
            (
              ex,
              lattice
                .getBlames(value)
                .filter(_.blamedPosition != idn)
                .map(lattice.injectBlame(_))
                .foldLeft(lattice.bottom)((a, b) => lattice.join(a, b))
            )
          case v => v
        }
        .filter {
          case (ExceptionAddr(_, _), v) => lattice.getBlames(v).nonEmpty
          case _                        => true
        }
    }

    def writeBlame(blame: Blame) =
      writeAddr(ExceptionAddr(component, expr(component).idn), lattice.injectBlame(blame))

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
        env.extend(lambda.variables.map(v => (v.name, allocVar(v, context(component)))))
    }

    /**
      * Computes the list of free variables of this component
      */
    def fv: Set[String] = expr(component).fv
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
      case (a, b) if a == b => a
      case (Top, _)         => Top
      case (_, Top)         => Top
      case (Bottom, _)      => b
      case (_, Bottom)      => a
      case (_, _)           => Top
    }
  }

  implicit def singleVerificationResultLattice = new Lattice[SingleVerificationResult] {
    type L = SingleVerificationResult

    /** A lattice has a bottom element */
    def bottom: L = Bottom

    /** A lattice has a top element (might be undefined) */
    def top: L = Top

    /** Elements of the lattice can be joined together */
    def join(x: L, y: => L): L = SingleVerificationResult.join(x, y)

    /** Subsumption between two elements can be checked */
    def subsumes(x: L, y: => L): Boolean = ???

    /** Equality check, returning an abstract result */
    def eql[B: BoolLattice](x: L, y: L): B = ???

    def show(v: SingleVerificationResult): String = v match {
      case Top           => "top"
      case Bottom        => "bottom"
      case VerifiedFalse => "#f"
      case VerifiedTrue  => "#t"
    }
  }

  case object Bottom        extends SingleVerificationResult
  case object VerifiedFalse extends SingleVerificationResult
  case object VerifiedTrue  extends SingleVerificationResult
  case object Top           extends SingleVerificationResult

  type VerifyIdentity = (Identity, Identity)
  var verificationResults: Map[Identity, Map[VerifyIdentity, SingleVerificationResult]] =
    Map().withDefaultValue(Map().withDefaultValue(Bottom))

  def addSingleVerificationResult(
      monIdn: Identity,
      contractIdn: VerifyIdentity,
      value: SingleVerificationResult
  ): Unit = {
    val monEntry      = verificationResults(monIdn)
    val contractEntry = monEntry(contractIdn)
    verificationResults += (monIdn -> (monEntry + (contractIdn ->
      SingleVerificationResult
        .join(contractEntry, value))))
  }

  def addVerified(monIdn: Identity, contractIdn: VerifyIdentity): Unit = {
    addSingleVerificationResult(monIdn, contractIdn, VerifiedTrue)
  }

  def addUnverified(monIdn: Identity, contractIdn: VerifyIdentity): Unit = {
    addSingleVerificationResult(monIdn, contractIdn, VerifiedFalse)
  }

  def getVerificationResults(
      result: SingleVerificationResult,
      context: Option[Identity] = None
  ): List[VerifyIdentity] = {
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

  type SMTSolver <: ScSmtSolver
  def newSmtSolver(program: ScExp): SMTSolver
}
