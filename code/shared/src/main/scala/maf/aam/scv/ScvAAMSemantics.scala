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
import maf.aam.scv.CallGraph.{Edge, Looped}

trait ScvAAMSemantics extends SchemeAAMSemantics:
    /** We add SCV specific information to the each AAM state */
    type Ext = ScvState
    type StoreCache = Map[Address, Symbolic]
    type PC = PathStore
    type Val = (LatVal, Option[Symbolic])

    /**
     * @param m
     *   the store cache, a mapping from addresses to symbolic values
     * @param phi
     *   the path condition, represented as a conjunction of symbolic constraints
     * @param vars
     *   a list of variables that are in the current path condition
     * @param graph
     *   a graph of the current calling history
     * @param pathStore
     *   a mapping from closures to path conditions
     * @param storeCacheClo
     *   a mapping from a closure to the lexical store cache
     */
    case class ScvState(
        m: StoreCache,
        phi: PC,
        vars: List[String],
        graph: CallGraph,
        pathStore: Map[(SchemeLambdaExp, Env), PC],
        storeCacheClo: Map[(SchemeLambdaExp, Env), StoreCache])

    /** Initial SCV state */
    def emptyExt: Ext =
      // TODO: put information about the primitivesi n the store cache initially
      ScvState(Map(), PathStore(), List(), CallGraph.empty, Map(), Map())

    // Errors
    case class Blame(blamer: Identity, blamed: Identity) extends SchemeError

    /*=============================================================================================================================*/
    /*===== Satisfiability (SMT) solver ===========================================================================================*/
    /*=============================================================================================================================*/

    /* Some primitives used for checking feasibility */
    private lazy val `true?` : Prim = primitives.allPrimitives("true?")
    private lazy val `false?` : Prim = primitives.allPrimitives("false?")

    protected lazy val satSolver: ScvSatSolver[LatVal]
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
    case class MonFrame(contract: SchemeExp, expression: SchemeExp, idn: Identity, env: Env, next: Option[KonA] = None) extends Frame:
        def link(kont: KonA): MonFrame = this.copy(next = Some(kont))

    /** Frame that gets pushed when `contract` is a flat contract and when we are evaluating the expression in the monitor expression */
    case class MonFlatFrame(contract: Flat[LatVal], expression: SchemeExp, idn: Identity, env: Env, next: Option[KonA] = None) extends Frame:
        def link(kont: KonA): MonFlatFrame = this.copy(next = Some(kont))

    /** Frame that gets pushed when trying to apply the flat contract to the value of the expression */
    case class MonFlatFrameRet(
        contract: Flat[LatVal],
        expression: SchemeExp,
        idn: Identity,
        vlu: Val,
        env: Env,
        next: Option[KonA] = None)
        extends Frame:
        def link(kont: KonA): MonFlatFrameRet = this.copy(next = Some(kont))

    case class MonFunFrame(contract: Grd[LatVal], epx: SchemeExp, idn: Identity, env: Env, next: Option[KonA] = None) extends Frame:
        def link(kont: KonA): MonFunFrame = this.copy(next = Some(kont))

    /** Frame that gets pushed when we evaluate an expression in a (flat expression)/idn expression */
    case class FlatLitFrame(exp: SchemeExp, idn: Identity, env: Env, next: Option[KonA] = None) extends Frame:
        def link(kont: KonA): FlatLitFrame = this.copy(next = Some(kont))

    /** Frame that signifies what to do after the range maker of a monitored function is applied */
    case class ArrRangeMakerFrame(fexp: SchemeFuncall, arr: Arr[LatVal], argv: List[Val], env: Env, next: Option[KonA] = None) extends Frame:
        def link(kont: KonA): ArrRangeMakerFrame = this.copy(next = Some(kont))

    /** Continuation frame to restore the context (path condition, store cache, call graph) after function call */
    case class RestoreCtxFrame(phi: PC, m: StoreCache, graph: CallGraph, looped: Boolean, next: Option[KonA] = None) extends Frame:
        def link(kont: KonA): RestoreCtxFrame = this.copy(next = Some(kont))

    /** Continuation frame to continue checking the domain contracts of a function guarded by a contract */
    case class CheckDomainFrame(
        fexp: SchemeFuncall,
        remainingArgv: List[Val],
        remainingSyntacticArguments: List[SchemeExp],
        arr: Arr[LatVal],
        rangeContract: Val,
        remainingDomains: List[LatVal],
        argv: List[Val],
        env: Env,
        next: Option[KonA] = None)
        extends Frame:
        def link(kont: KonA): CheckDomainFrame = this.copy(next = Some(kont))

    case class DepContractFrame(
        domains: List[SchemeExp],
        rangeMaker: Option[SchemeExp],
        domainsV: List[Val],
        rangeMakerV: Option[Val],
        domainIdn: List[Identity],
        rangeMakerExp: SchemeExp,
        env: Env,
        next: Option[KonA] = None)
        extends Frame:
        def link(kont: KonA): DepContractFrame = this.copy(next = Some(kont))

    /*=============================================================================================================================*/
    /* ===== Extension points =====================================================================================================*/
    /*=============================================================================================================================*/

    override protected def cond(
        value: Val,
        csq: Expr,
        alt: Expr,
        env: Env,
        sto: Sto,
        kont: KonA,
        t: Timestamp,
        ext: Ext
      ): Set[State] =
        // [CondTrue] for comparison wih the paper, "nonzero?" has been renamed to "true?"
        val csqSt =
          feasible(`true?`, value, ext.phi, ext.vars)
            .map(phi1 => Set(ev(csq, env, sto, kont, t, ext.copy(phi = phi1))))
            .getOrElse(Set())

        // [CondTrue] for comparison wih the paper, "zero?" has been renamed to "false?"
        val altSt =
          feasible(`false?`, value, ext.phi, ext.vars)
            .map(phi1 => Set(ev(alt, env, sto, kont, t, ext.copy(phi = phi1))))
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
        kon: KonA,
        t: Timestamp,
        ext: Ext
      ): Set[State] =
      // apply the primitive and then add a symbolic application to the symbolic value
      super.applyPrim(fexp, func, argv, env, sto, kon, t, ext).map(_.mapValue(tagOption(ap(fexp, argv))))

    override protected def applyClo(
        fexp: SchemeFuncall,
        func: Val,
        argv: List[Val],
        env: Env,
        sto: Sto,
        kon: KonA,
        t: Timestamp,
        ext: Ext
      ): Set[State] =
      lattice.getClosures(project(func)).flatMap {
        case (lam, lex: Env @unchecked) if lam.check(argv.size) =>
          val (env1, sto2, t0) = bindArgs(fexp, argv, lam, lex, sto, kon, t)

          // update the path condition and store cache, only share the path condition
          // and store cache if it was not looped.
          val (ext1, looped) = ext.graph.add(Edge((lam, lex))) match
              case Looped.Safe(c) =>
                (ext.copy(graph = c), false) // TODO: gc store cache?

              // Default behaviour from the paper: use the path condition and store
              // cache from the closure. But conservatively update the store cache
              // such that free variables are removed.
              case Looped.Recursive(_) =>
                // TODO: restore m and phi after function application
                val m1 = ext.storeCacheClo((lam, lex)) -- lam.fv.map(lex.lookup(_).get)
                (ext.copy(phi = phiOfClo((lam, lex), ext), m = m1), true)

          // then, add symbolic representations for the arguments of the function
          val fixd = argv.take(lam.args.size) // only provide sym representation for fixed arguments
          val (syms, ext2) = fixd.foldRight((List[Symbolic](), ext)) {
            case ((_, Some(sym)), (syms, ext)) => (sym :: syms, ext)
            case ((_, None), (syms, ext)) =>
              val (sym, ext1) = fresh(ext)
              (sym :: syms, ext1)
          }

          // finally, bind them in the store cache
          val ext3 = ext2.copy(m = ext1.m ++ lam.args.zip(syms).map { case (arg: Identifier, sym) =>
            // TODO: check if this is correct and ensures a terminating analysis, maybe we need to clean up the store cache
            env1(arg.name) -> sym
          })

          // push a return continuation on the continuation stack to restore the path condition
          // and the store cache
          val (sto3, frame, t1) = pushFrameRet(fexp, env, sto, kon, RestoreCtxFrame(ext.phi, ext.m, ext.graph, looped), t0)

          // and evaluate the body
          evaluate_sequence(env1, sto3, frame, lam.body, t1, ext3, true)
        case (lam, lex) =>
          invalidArity(fexp, argv.size, lam.args.size + lam.varArgId.size, sto, kon, t, ext)
      }

    override protected def applyFun(
        fexp: SchemeFuncall,
        func: Val,
        argv: List[Val],
        env: Env,
        sto: Sto,
        kon: KonA,
        t: Timestamp,
        ext: Ext
      ): Set[State] =
        val others = super.applyFun(fexp, func, argv, env, sto, kon, t, ext)
        val arrs = applyArrs(fexp, func, argv, env, sto, kon, t, ext)
        others ++ arrs

    override def eval(
        exp: SchemeExp,
        env: Env,
        sto: Sto,
        kont: KonA,
        t: Timestamp,
        ext: Ext
      ): Set[State] = exp match
        // [Lit]
        case lit: SchemeValue =>
          super.evalLiteralVal(lit, env, sto, kont, t, ext).map(_.mapValue(tag(lit)))

        case lam: SchemeLambdaExp =>
          val clo = (lam, env.restrictTo(lam.fv))

          // register the lexical path condition and store cache that will be used at application time
          val ext1 = ext.copy(
            pathStore = ext.pathStore + (clo -> ext.phi),
            storeCacheClo = ext.storeCacheClo + (clo -> ext.m)
          )

          Set(ap(inject(lattice.closure(clo)), sto, kont, t, ext1))

        case ContractSchemeMon(contract, expression, idn) =>
          mon(contract, expression, idn, env, sto, kont, t, ext)

        case ContractSchemeDepContract(domains, rangeMaker, idn) =>
          evaluateDepContract(domains, Some(rangeMaker), rangeMaker, env, sto, kont, t, ext)

        case ContractSchemeFlatContract(flat, idn) =>
          val (sto1, frame, t1) = pushFrame(flat, env, sto, kont, FlatLitFrame(flat, idn, env), t)
          Set(ev(flat, env, sto1, frame, t1, ext))
        case _ => super.eval(exp, env, sto, kont, t, ext)

    override def continue(vlu: Val, sto: Sto, kon: KonA, t: Timestamp, ext: Ext): Set[State] =
      readKonts(sto, kon).flatMap {
        case MonFrame(contract, expression, idn, env, Some(next)) =>
          applyMon(vlu, expression, None, idn, env, sto, next, ext, t)

        // [MonFlat]
        case MonFlatFrame(contract, expression, idn, env, Some(next)) =>
          val fexp = SchemeFuncall(contract.fexp, List(expression), idn)
          val func = contract.contract
          val args = List(vlu)
          // TODO: fix expression here so that we maintain proper call-return matching
          val (sto1, frame, t1) =
            pushFrame(SchemeValue(Value.Nil, Identity.none), env, sto, next, MonFlatFrameRet(contract, expression, idn, vlu, env), t)
          applyFun(fexp, inject(func), args, env, sto1, frame, t1, ext)

        // [MonFlat] checking whether expression satisfies contract, otherwise blame
        case MonFlatFrameRet(contract, expression, idn, expVlu, env, Some(next)) =>
          val nonblames = feasible(`true?`, vlu, ext.phi, ext.vars)
            .map((phi1) => Set(ap(expVlu, sto, next, t, ext.copy(phi = phi1))))
            .getOrElse(Set())

          val blames = feasible(`false?`, vlu, ext.phi, ext.vars)
            .map((phi1) => Set(blame(contract.contractIdn, expression.idn, sto, next, t, ext)))
            .getOrElse(Set())

          nonblames ++ blames

        // [MonFun]
        case MonFunFrame(contract, expression, idn, env, Some(next)) =>
          val arr = Arr(idn, expression.idn, contract, project(vlu))
          Set(ap(inject(lattice.arr(arr)), sto, kon, t, ext))

        // Rule added to evaluate flat contracts
        case FlatLitFrame(exp, idn, env, Some(next)) =>
          Set(ap(inject(lattice.flat(Flat(project(vlu), exp, vlu._2.map(_.expr), idn))), sto, next, t, ext))

        // [MonArr]
        case ArrRangeMakerFrame(fexp, arr, argv, env, Some(next)) =>
          // The value of the range contract is in the value passed to this continuation
          // Now apply mon on the arguments of the function to the domain contracts
          checkDomains(fexp, fexp.args, arr, vlu, arr.contract.domain, argv, List(), env, sto, next, t, ext)

        // Restore the context after a function call
        case RestoreCtxFrame(phi, m, graph, looped, Some(next)) =>
          Set(
            ap(
              vlu,
              sto,
              kon,
              t,
              // only restore path condition and store cache if the function call was looping
              if looped then ext.copy(phi = phi, m = m)
              else ext.copy(graph = graph.pop)
            )
          )

        case CheckDomainFrame(fexp, remainingArgv, remainingSyntacticArguments, arr, rangeContract, remainingDomains, argv, env, Some(next)) =>
          checkDomains(fexp, remainingSyntacticArguments, arr, rangeContract, remainingDomains, remainingArgv, vlu :: argv, env, sto, next, t, ext)

        case DepContractFrame(domains, rangeMaker, domainsV, rangeMakerV, domainIdn, rangeMakerExp, env, Some(next)) =>
          evaluateDepContract(
            domains,
            rangeMaker,
            rangeMakerExp,
            env,
            sto,
            kon,
            t,
            ext,
            if rangeMaker.isDefined then vlu :: domainsV else domainsV,
            if rangeMaker.isEmpty then Some(vlu) else rangeMakerV,
            domainIdn
          )

        case _ => super.continue(vlu, sto, kon, t, ext)
      }

    /*=============================================================================================================================*/

    /** Generate a fresh symbolic variable */
    private def fresh(ext: Ext): (Symbolic, Ext) =
        val next = ext.vars.size + 1
        val newName = s"x$next"
        (Symbolic(SchemeVar(Identifier(newName, Identity.none))), ext.copy(vars = newName :: ext.vars))

    /** Lookup the path condition of the given clojure */
    protected def phiOfClo(clo: (SchemeLambdaExp, Env), ext: Ext): PC =
      // SAFETY: .get will never result in an exception since
      // the path condition is set when the clojure is defined
      ext.pathStore.get(clo).get

    protected def blame(
        blamer: Identity,
        blamed: Identity,
        sto: Sto,
        kon: KonA,
        t: Timestamp,
        extra: Ext
      ): State =
      SchemeState(Control.HltE(Blame(blamer, blamed)), sto, kon, t, extra)

    /**
     * Provides the semantics for a `mon` expression.
     *
     * Given the expression `(mon contract expression)`. Then we required the following parameters:
     *
     * @param contract
     *   the contract of the mon expression
     * @param expression
     *   the expression of the mon expression
     * @param idn
     *   the identity of the entire mon expression
     */
    protected def mon(
        contract: SchemeExp,
        expression: SchemeExp,
        idn: Identity,
        env: Env,
        sto: Sto,
        kont: KonA,
        t: Timestamp,
        ext: Ext
      ): Set[State] =
        val (sto1, frame, t1) = pushFrame(contract, env, sto, kont, MonFrame(contract, expression, idn, env), t)
        Set(ev(contract, env, sto, kont, t, ext))

    /** Create a new post value from the given value where the symbolic representation is given by the given `SchemeExp` */
    protected def tag(e: SchemeExp)(v: Val): Val =
      (v._1, Some(Symbolic(e)))

    protected def tagOption(e: Option[SchemeExp])(v: Val): Val = e match
        case Some(exp) => tag(exp)(v)
        case None      => v

    protected def evaluateDepContract(
        remainingDomains: List[SchemeExp],
        rangeMaker: Option[SchemeExp],
        rangeMakerExp: SchemeExp,
        env: Env,
        sto: Sto,
        kon: KonA,
        t: Timestamp,
        ext: Ext,
        domainsV: List[Val] = List(),
        rangeMakerV: Option[Val] = None,
        domainsIdn: List[Identity] = List(),
      ): Set[SchemeState] = (remainingDomains, rangeMaker) match
        case (domain :: domains, _) =>
          val next = DepContractFrame(domains, rangeMaker, domainsV, rangeMakerV, domain.idn :: domainsIdn, rangeMakerExp, env).link(kon)
          Set(ev(domain, env, sto, next, t, ext))
        case (List(), Some(rangeContract)) =>
          val next = DepContractFrame(List(), None, domainsV, rangeMakerV, domainsIdn, rangeMakerExp, env).link(kon)
          Set(ev(rangeContract, env, sto, next, t, ext))
        case (List(), None) =>
          Set(ap(inject(lattice.grd(Grd(domainsV.map(project), project(rangeMakerV.get), domainsIdn, rangeMakerExp))), sto, kon, t, ext))

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

    /** Semantics for applying a function that is monitored by a contract */
    protected def applyArrs(
        fexp: SchemeFuncall,
        func: Val,
        argv: List[Val],
        env: Env,
        sto: Sto,
        kon: KonA,
        t: Timestamp,
        ext: Ext
      ): Set[State] =
      lattice.getArrs(project(func)).flatMap { arr =>
          val contract = arr.contract
          // first check whether a sufficient number of arguments was provided
          if arr.checkArgs(argv) then
              // then pass the function values to the rangeMaker
              // TODO: fexp is probably not the right choice here as a return address
              val (sto1, frame, t1) = pushFrame(fexp, env, sto, kon, ArrRangeMakerFrame(fexp, arr, argv, env), t)
              applyFun(fexp, inject(contract.rangeMaker), argv, env, sto, kon, t, ext)
          else invalidArity(fexp, argv.size, arr.expectedNumArgs, sto, kon, t, ext)
      }

    protected def applyMon(
        contractVlu: Val,
        expression: SchemeExp,
        expressionVlu: Option[Val],
        monIdn: Identity,
        env: Env,
        sto: Sto,
        next: KonA,
        ext: Ext,
        t: Timestamp
      ): Set[State] =
        // feasible(phi, flat-contract?, w', phi') -> change: do not extend PC with this information
        val flats = lattice
          .getFlats(project(contractVlu))
          .map((flat) => {
            val newFrame = MonFlatFrame(flat, expression, monIdn, env)
            val (sto1, frame, t1) = pushFrame(expression, env, sto, next, newFrame, t)
            expressionVlu match
                case Some(vlu) => ap(vlu, sto1, frame, t1, ext) // continue if we have an exp value
                case None      => ev(expression, env, sto1, frame, t1, ext) // evaluate exp value first
          })

        // feasible(phi, dep-contract?, w', phi') -> change: do not extend PC with this information
        val grds = lattice
          .getGrds(project(contractVlu))
          .map((grd) =>
              val newFrame = MonFunFrame(grd, expression, monIdn, env)
              val (sto1, frame, t1) = pushFrame(expression, env, sto, next, newFrame, t)
              expressionVlu match
                  case Some(vlu) => ap(vlu, sto1, frame, t1, ext)
                  case None      => ev(expression, env, sto1, frame, t1, ext)
          )

        grds ++ flats

    /** Given a list of remaining domains to check, check the domain contracts on the given values */
    protected def checkDomains(
        fexp: SchemeFuncall,
        remainingSyntacticArguments: List[SchemeExp],
        arr: Arr[LatVal],
        rangeContract: Val, /* The contract guarding the function */
        domains: List[LatVal], /* A list of domain contracts (remaining) */
        remainingArgv: List[Val], /* A list of arguments to the function (remaining) */
        argv: List[Val], /* The monitored set of arguments (initially empty) */
        env: Env,
        sto: Sto,
        kon: KonA,
        t: Timestamp,
        ext: Ext
      ): Set[State] = domains match
        case domain :: domains =>
          // push a frame so that the next domains can be evaluated
          val nextFrame =
            CheckDomainFrame(fexp, remainingArgv.tail, remainingSyntacticArguments.tail, arr, rangeContract, domains.tail, argv, env).link(kon)
          // validate the next domain contract
          applyMon(inject(domain), remainingSyntacticArguments.head, remainingArgv.headOption, fexp.idn, env, sto, kon, ext, t)

        case _ =>
          // if all the domain contracts have been validated, we can execute the monitored function
          val monitoredFunction = inject(arr.e)
          applyFun(fexp, monitoredFunction, argv, env, sto, kon, t, ext)
