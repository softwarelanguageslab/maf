package maf.aam.scheme.optimisations

import maf.aam.scheme.*

import maf.language.scheme.*
import maf.core.Identifier
import maf.util.Trampoline.*

/**
 * The optimisation here is that we do allocate the continuation to the other continuations in the case that we are evaluating an atom. This could
 * reduce the length of the linked list of continuations, and therefore the number of discrete continuations that could exist as seperate values in
 * the store.
 *
 * This optimization is especially useful in classic AAM, where no inter or intra analysis exists. The optimization would therefore introduce a
 * smaller number of states that in the state space, this in turn results in a lower possibility of branching, which therefore could stop the
 * exponentional growth of the state space.
 */
trait SchemeAtomicEvaluation extends BaseSchemeAAMSemantics:
    override protected def pushFrameEv(
        e: Expr,
        env: Env,
        sto: Sto,
        next: KonA,
        frame: Kont,
        t: Timestamp,
        ext: Ext,
        call: Boolean = false
      ): Result = e match
        case lit: SchemeValue =>
          val (res, sto1, ext1) = super.evalLiteralValToVal(lit, env, sto, frame.link(next), t, ext)
          tailcall(continue(inject(res), sto1, frame.link(next), t, ext))
        case SchemeVar(id) =>
          val (vlu, sto1): (Val, Sto) = env
            .lookup(id.name)
            .map(readStoV(sto, _, ext))
            .getOrElse {
              println(s"ERR: undefined variable $id")
              (inject(lattice.bottom), sto)
            }

          tailcall(continue(vlu, sto1, frame.link(next), t, ext))

        case _ => super.pushFrameEv(e, env, sto, next, frame, t, ext, call)
