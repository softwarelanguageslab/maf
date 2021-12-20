package maf.aam.scheme.optimisations

import maf.util.Trampoline.{done, given}
import maf.core.Monad.*
import maf.core.{Address, Identity}

/**
 * In small step semantics we need to link another continuation before evaluating the consequent and alternative such that a widening is done of the
 * return value in the global store on the address of the if expression itself.
 *
 * Importantly, we need to return this apply state back to the worklist loop because otherwise the widening will have no effect as both program paths
 * will run in the intra-analysis regardless.
 */
trait SchemeWideningAfterCondition extends SchemeFunctionCallBoundary:
    case class WidenConditionalFrame(
        ifExprIdn: Identity,
        /** Remaining states */
        next: Option[KonA] = None)
        extends Frame:

        def link(next: KonA): WidenConditionalFrame = this.copy(next = Some(next))

    override protected def cond(
        value: Val,
        csq: Expr,
        alt: Expr,
        env: Env,
        sto: Sto,
        kont: KonA,
        t: Timestamp,
        ext: Ext,
        ifIdn: Identity
      ): Result =
        import Control.*
        // before evaluating the consequent and/or alternatives. Insert a continuation
        // that joins the values together on the same address in the store.
        val frame = WidenConditionalFrame(ifIdn).link(kont)
        super.cond(value, csq, alt, env, sto, frame, t, ext, ifIdn)

    override def continue(value: Val, sto: Sto, kont: KonA, t: Timestamp, ext: Ext): Result =
      readKonts(sto, kont).map { (kont, sto) =>
        kont match
            case WidenConditionalFrame(idn, Some(next)) =>
              val addr = WidenAddr(idn, t)
              // write the result to that address
              val (sto1, ext1) = writeStoV(sto, addr, value, ext)
              // use that address as a return value, in the global store case, this
              // will make the state of the alternative and consequence the same before
              // the actual divergent paths are explored.
              done(Set(SchemeState(Control.Ret(addr), sto1, next, t, ext1)))
            case _ => super.continue(value, sto, kont, t, ext)
      }.flattenM
