package maf.aam.scheme.optimisations

import maf.language.scheme.SchemeLambdaExp
import maf.util.Trampoline.done
import maf.aam.scheme.stores.SchemeImperativeStoreWidening
import maf.language.scheme.SchemeExp
import maf.aam.scheme.SchemeStoreAllocateReturn
import maf.core.Monad.MonadSyntaxOps
import maf.core.Monad.*

/**
 * Behaves like ModF in the sense that it uses an effect driven global store and analyses the continuation of a function directly instead of analysing
 * the function body first
 */
trait SchemeFunctionModularAAM extends SchemeImperativeStoreWidening, SchemeStoreAllocateReturn:
    /** Frame that is at the bottom of a function application */
    case class RetFrame(lam: SchemeLambdaExp, ctx: Timestamp) extends Frame:
        def link(next: KonA): RetFrame =
          throw new Exception("cannot link to a bottom frame (RetFrame)")

        override def stackString: String = s"<retfn $lam $ctx>"

    override def continue(value: Val, sto: Sto, kont: KonA, t: Timestamp, ext: Ext): Result =
      readKonts(sto, kont).map { case (kont, sto) =>
        kont match
            case RetFrame(_, _) => done(Set(SchemeState(Control.RetFn, sto, kont, t, ext)))
            case _              => super.continue(value, sto, kont, t, ext)
      }.flattenM

    override def call(
        lam: SchemeLambdaExp,
        env: Env,
        sto: Sto,
        kon: KonA,
        body: List[SchemeExp],
        t: Timestamp,
        ext: Ext
      ): Result =
        val retKon = RetFrame(lam, t)
        for {
          // instead of directly returning the config we store it for later analysis
          // we also wipe out the continuation as an analysis of this state will be retriggered when necessary
          callConf <- evaluate_sequence(env, sto, retKon, body, t, ext, true)
          (v, sto1): (Val, Sto) = readStoV(sto, super[SchemeStoreAllocateReturn].allocRet(retKon), ext)
          // also register the call effect
          sto2 = sto1.callDep(callConf)

          // cut off the analysis if the value is bottom
          result <-
            if lattice.isBottom(project(v)) then done(Set(SchemeState(Control.RetFn, sto2, kon, t, ext)))
            else
                // continue with the continuation of the function, using an (old) value from the global store
                continue(v, sto2, kon, t, ext)
        } yield result

    override def ap(value: Val, sto: Sto, kont: KonA, t: Timestamp, ext: Ext): Result =
      kont match
          case _ if readKonts(sto, kont).map(_._1).collectFirst { case _: RetFrame => }.isDefined =>
            super[SchemeStoreAllocateReturn].ap(value, sto, kont, t, ext)
          case _ => super.ap(value, sto, kont, t, ext)
