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
import maf.language.ContractScheme.ContractValues.*
import maf.modular.scv.ScvSatSolver
import maf.language.scheme.SchemeValue

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

    def ap(fexp: SchemeFuncall, argv: List[Val]): Option[SchemeExp] =
        import maf.core.Monad.MonadIterableOps
        import maf.core.OptionMonad.given
        for
            args <- argv.foldRightM(List[SchemeExp]())((arg, symArgs) => arg._2.map(sym => sym.expr :: symArgs))
            symbolicExp = SchemeFuncall(fexp.f, args, Identity.none)
        yield symbolicExp

    /*=============================================================================================================================*/
    /* ===== Post values ==========================================================================================================*/
    /*=============================================================================================================================*/

    override def inject(v: LatVal): Val =
      // by default we will just inject it with the empty smbolic representation
      (v, None)

    override def project(v: Val): LatVal =
      v._1

    /*=============================================================================================================================*/
    /* ===== Continuations ==========================================================================================================*/
    /*=============================================================================================================================*/

    /** Evaluation of (mon contract expression)/idn */
    case class MonFrame(contract: SchemeExp, expression: SchemeExp, idn: Identity, env: Env, next: Option[Address] = None) extends Frame:
        def link(kont: Address): MonFrame = this.copy(next = Some(kont))

    /** Frame that gets pushed when `contract` is a flat contract and when we are evaluating the expression in the monitor expression */
    case class MonFlatFrame(contract: Flat[LatVal], idn: Identity, env: Env, next: Option[Address] = None) extends Frame:
        def link(kont: Address): MonFlatFrame = this.copy(next = Some(kont))

    /** Frame that gets pushed when trying to apply the flat contract to the value of the expression */
    case class MonFlatFrameRet(contract: Flat[LatVal], idn: Identity, vlu: Val, env: Env, next: Option[Address] = None) extends Frame:
        def link(kont: Address): MonFlatFrameRet = this.copy(next = Some(kont))

    case class MonFunFrame(contract: Grd[LatVal], idn: Identity, env: Env, next: Option[Address] = None) extends Frame:
        def link(kont: Address): MonFunFrame = this.copy(next = Some(kont))

    /** Frame that gets pushed when we evaluate an expression in a (flat expression)/idn expression */
    case class FlatLitFrame(exp: SchemeExp, idn: Identity, env: Env, next: Option[Address] = None) extends Frame:
        def link(kont: Address): FlatLitFrame = this.copy(next = Some(kont))

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

    override def applyPrim(
        fexp: SchemeFuncall,
        func: Val,
        argv: List[Val],
        env: Env,
        sto: Sto,
        kon: Address,
        t: Timestamp,
        ext: Ext
      ): Set[State] =
      super.applyPrim(fexp, func, argv, env, sto, kon, t, ext).map(_.mapValue(tagOption(ap(fexp, argv))))

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

        case ContractSchemeMon(contract, expression, idn) =>
          mon(contract, expression, idn, env, sto, kont, t, ext)

        case ContractSchemeFlatContract(flat, idn) =>
          val (sto1, frame, t1) = pushFrame(flat, env, sto, kont, FlatLitFrame(flat, idn, env), t)
          Set(SchemeState(Control.Ev(flat, env), sto1, frame, t1, ext))

        case _ => super.eval(exp, env, sto, kont, t, ext)

    override def continue(vlu: Val, sto: Sto, kon: Address, t: Timestamp, ext: Ext): Set[State] =
      readKonts(sto, kon).flatMap {
        case MonFrame(contract, expression, idn, env, Some(next)) =>
          // feasible(phi, flat-contract?, w', phi') -> change: do not extend PC with this information
          val flats = lattice
            .getFlats(project(vlu))
            .map((flat) => {
              val (sto1, frame, t1) = pushFrame(expression, env, sto, next, MonFlatFrame(flat, idn, env), t)
              SchemeState(Control.Ev(expression, env), sto1, frame, t1, ext)
            })

          // feasible(phi, dep-contract?, w', phi') -> change: do not extend PC with this information
          val grds = lattice
            .getGrds(project(vlu))
            .map((grd) =>
                val (sto1, frame, t1) = pushFrame(expression, env, sto, next, MonFunFrame(grd, idn, env), t)
                SchemeState(Control.Ev(expression, env), sto1, frame, t1, ext)
            )

          grds ++ flats

        // [MonFlat]
        case MonFlatFrame(contract, idn, env, Some(next)) =>
          val fexp = SchemeFuncall(contract.fexp, ???, ???) // TODO: add expression expr to continuation
          val func = contract.contract
          val args = List(vlu)
          // TODO: fix expression here so that we maintain proper call-return matching
          val (sto1, frame, t1) = pushFrame(SchemeValue(Value.Nil, Identity.none), env, sto, next, MonFlatFrameRet(contract, idn, vlu, env), t)
          applyFun(fexp, inject(func), args, env, sto1, frame, t1, ext)

        // [MonFlat] checking whether expression satisfies contract, otherwise blame
        case MonFlatFrameRet(contract, idn, expVlu, env, Some(next)) =>
          val nonblames = feasible(`true?`, vlu, ext.phi, ext.vars)
            .map((phi1) => Set(SchemeState(Control.Ap(expVlu), sto, next, t, ext.copy(phi = phi1))))
            .getOrElse(Set())

          val blames = feasible(`false?`, vlu, ext.phi, ext.vars)
            .map((phi1) => Set(blame(contract.contractIdn, ???))) // TODO: we also need the identity of the expression to blame
            .getOrElse(Set())

          nonblames ++ blames

        // [MonFun]
        case MonFunFrame(contract, idn, env, Some(next)) =>
          ???

        // Rule added to evaluate flat contracts
        case FlatLitFrame(exp, idn, env, Some(next)) =>
          Some(SchemeState(Control.Ap(inject(lattice.flat(Flat(project(vlu), exp, vlu._2.map(_.expr), idn)))), sto, next, t, ext))

        case _ => super.continue(vlu, sto, kon, t, ext)
      }

    /*=============================================================================================================================*/

    protected def blame(blamer: Identity, blamed: Identity): State = ???

    protected def mon(
        contract: SchemeExp,
        expression: SchemeExp,
        idn: Identity,
        env: Env,
        sto: Sto,
        kont: Address,
        t: Timestamp,
        ext: Ext
      ): Set[State] =
        val (sto1, frame, t1) = pushFrame(contract, env, sto, kont, MonFrame(contract, expression, idn, env), t)
        Set(SchemeState(Control.Ev(contract, env), sto, kont, t, ext))

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
