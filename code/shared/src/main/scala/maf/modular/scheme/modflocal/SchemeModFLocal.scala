package maf.modular.scheme.modflocal

/*
import maf.modular.ModAnalysis

import maf.modular.scheme._
import maf.core.Position._
import maf.core._
import maf.language.scheme._
import maf.language.scheme.primitives._
import maf.language.sexp
import maf.modular.components._
import maf.util.benchmarks.Timeout

trait KontStore[Addr, Kont] extends Store[Addr, Kont]

trait SchemeModFLocal extends ModAnalysis[SchemeExp]
                         with ContextSensitiveComponents[SchemeExp]
                         with SchemeDomain {

    // shorthands
    type Exp = SchemeExp
    type Clo = lattice.Closure
    type Env = Environment[Address]
    type Sto = Store[Address, Value]
    type KSt = KontStore[Address, Kon]
    type Kon = List[Frame]
    type Ctx = Option[ComponentContext]

    sealed trait Component extends Serializable
    case object MainComponent extends Component
    case class CallComponent(clo: Clo, sto: Sto, kst: KSt, ctx: Ctx) extends Component 

    lazy val mainBody: Exp = program
    lazy val initialEnv: Env = ??? 
    lazy val initialSto: Sto = ??? 
    lazy val initialKst: KSt = ???

    sealed trait Frame
    case object Halt extends Frame

    private def eval(
        exp: Exp,
        env: Env,
        sto: Sto,
        kst: KSt,
        kon: Kon,
        ctx: Ctx
    ): Unit = ???

    private def evalSequence(
        eps: List[Exp],
        env: Env,
        sto: Sto,
        kst: KSt,
        kon: Kon,
        ctx: Ctx
    ): Unit = ???
    
    override def intraAnalysis(cmp: Component) = new SchemeIntraAnalysis(cmp)
    class SchemeIntraAnalysis(cmp: Component) extends IntraAnalysis(cmp) {
        def analyzeWithTimeout(timeout: Timeout.T): Unit = cmp match {
            case MainComponent => 
                eval(mainBody, initialEnv, initialSto, initialKst, List(Halt), None)
            case CallComponent((lam, env), sto, kst, ctx) =>
                evalSequence(lam.body, )
        }
    } 

}
*/