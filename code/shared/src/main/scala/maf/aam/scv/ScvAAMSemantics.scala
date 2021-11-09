package maf.aam.scv

import maf.aam.scheme.*
import maf.core.Address
import maf.core.{Identifier, Identity}
import maf.modular.scv.*
import maf.modular.scv.Symbolic.Implicits
import maf.language.scheme.*
import maf.language.scheme.primitives.*
import maf.language.sexp.*
import maf.language.ContractScheme.*
import maf.modular.scv.ScvSatSolver

trait ScvAAMSemantics extends SchemeAAMSemantics:
    /** We add SCV specific information to the each AAM state */
    type Ext = ScvState
    type StoreCache = Map[Address, Symbolic]
    type PC = PathStore
    type Val = (LatVal, Option[Symbolic])

    case class ScvState(m: StoreCache, phi: PC, vars: List[String])

    /*=============================================================================================================================*/
    /*===== Satisfiability (SMT) solver ===========================================================================================*/
    /*=============================================================================================================================*/

    /* Some primitives used for checking feasibility */
    private lazy val `true?` : Prim = primitives.allPrimitives("true?")
    private lazy val `false?` : Prim = primitives.allPrimitives("false?")

    protected val satSolver: ScvSatSolver[LatVal]
    protected def checkPc(phi: PC, variables: List[String]): Boolean =
      satSolver.feasible(phi.pc, variables)

    /*=============================================================================================================================*/
    /* ===== Post values ==========================================================================================================*/
    /*=============================================================================================================================*/

    override def inject(v: LatVal): Val =
      // by default we will just inject it with the empty smbolic representation
      (v, None)

    override def project(v: Val): LatVal =
      v._1

    /*=============================================================================================================================*/
    /* ===== Extension points =====================================================================================================*/
    /*=============================================================================================================================*/

    override protected def cond(
        value: Val,
        csq: Expr,
        alt: Expr,
        env: Env,
        sto: Sto,
        kont: Address,
        t: Timestamp,
        ext: Ext
      ): Set[State] =
        // [CondTrue] for comparison wih the paper, "nonzero?" has been renamed to "true?"
        val csqSt =
          feasible(`true?`, value, ext.phi, ext.vars)
            .map(phi1 => Set(SchemeState(Control.Ev(csq, env), sto, kont, t, ext.copy(phi = phi1))))
            .getOrElse(Set())

        // [CondTrue] for comparison wih the paper, "zero?" has been renamed to "false?"
        val altSt =
          feasible(`false?`, value, ext.phi, ext.vars)
            .map(phi1 => Set(SchemeState(Control.Ev(csq, env), sto, kont, t, ext.copy(phi = phi1))))
            .getOrElse(Set())

        csqSt ++ altSt

    override def readStoV(sto: Sto, addr: Address, ext: Ext): Val =
      tagOption(ext.m.get(addr).map(_.expr))(super.readStoV(sto, addr, ext))

    override def eval(
        exp: SchemeExp,
        env: Env,
        sto: Sto,
        kont: Address,
        t: Timestamp,
        ext: Ext
      ): Set[State] = exp match
        // [Lit]
        case lit: SchemeValue =>
          super.evalLiteralVal(lit, env, sto, kont, t, ext).map(_.mapValue(tag(lit)))

        case _ => super.eval(exp, env, sto, kont, t, ext)

    override def continue(vlu: Val, sto: Sto, kon: Address, t: Timestamp, ext: Ext): Set[State] =
      super.continue(vlu, sto, kon, t, ext)

    /*=============================================================================================================================*/

    /** Create a new post value from the given value where the symbolic representation is given by the given `SchemeExp` */
    protected def tag(e: SchemeExp)(v: Val): Val =
      (v._1, Some(Symbolic(e)))

    protected def tagOption(e: Option[SchemeExp])(v: Val): Val = e match
        case Some(exp) => tag(exp)(v)
        case None      => v

    /** Check whether the given value possibly satisfies the given condition */
    protected def feasible(cond: SchemePrimitive[LatVal, Address], value: Val, phi: PC, vars: List[String]): Option[PC] =
        val res = callPrimitive(SchemeValue(Value.Nil, Identity.none), cond, List(value))
        if lattice.isTrue(res) then
            for {
              sym <- value._2
              arg = sym.expr
              phi1 = phi.extendPc(Symbolic(SchemeFuncall(SchemeVar(Identifier(cond.name, Identity.none)), List(arg), Identity.none)))
              if checkPc(phi1, vars)
            } yield phi1
        else None
