package maf.modular.scheme.modactor

import maf.core.*
import maf.modular.*
import maf.modular.scheme.modf.*
import maf.core.SetMonad.*
import maf.language.scheme.SchemeExp

trait SchemeModActorInnerMonad[Msg] extends ModAnalysis[SchemeExp] with BaseSchemeModFSemanticsM with BigStepModFSemanticsT:
    outer =>
    import maf.core.Monad.MonadSyntaxOps

    protected type Mailbox = AbstractMailbox[Msg]

    case class State(env: Environment[Address], mailbox: Mailbox):
        def enqueue(m: Msg): State = this.copy(mailbox = this.mailbox.enqueue(m))

    override type EvalM[X] = AEvalM[X]
    type AEvalM[X] = MonadStateT[State, Set, X]
    final lazy val aevalM = MonadStateT.stateInstance[State, Set]

    def withEnvM[X](f: Environment[Address] => EvalM[Environment[Address]])(ev: EvalM[X]): EvalM[X] =
        import aevalM.{get, impure, put, withState}

        for
            s <- get
            oldEnv = s.env
            newEnv <- f(oldEnv)
            s1 <- get
            _ <- put(s1.copy(env = newEnv))
            result <- ev
            newSt <- get
            _ <- put(newSt.copy(env = oldEnv))
        yield result

    def nondets[X](xs: Set[EvalM[X]]): EvalM[X] =
        MonadStateT((state) => xs.flatMap(_.run(state)))

    def nondet[X](x: EvalM[X], y: EvalM[X]): EvalM[X] =
        nondets(Set(x, y))

    /** Non deterministic receive of a message */
    def pop: EvalM[Msg] =
        import aevalM.*
        get.map(_.mailbox.pop).flatMap(msgs => nondets(msgs.map(unit)))

    def remove(msg: Msg): EvalM[Unit] =
        import aevalM.*
        for
            st <- get
            stNew = st.copy(mailbox = st.mailbox.remove(msg))
            _ <- put(st)
        yield ()

    implicit val evalM = new TEvalM:
        import aevalM.{get, impure, put, withState}
        export aevalM._
        def getEnv: EvalM[Environment[Address]] = get.map(_.env)
        def withEnv[X](f: Environment[Address] => Environment[Address])(ev: => EvalM[X]): EvalM[X] =
            outer.withEnvM(f andThen unit)(ev)

        //def guard(bln: Boolean): EvalM[Unit] =
        //  if bln then unit(()) else mzero
        def mzero[X]: EvalM[X] = MonadStateT.lift(Set.empty)
        def merge[X: Lattice](x: EvalM[X], y: EvalM[X]): EvalM[X] =
            // two programs paths are not merged together in Scv but are rather explorered seperately
            nondet(x, y)
        def fail[X](e: Error): EvalM[X] =
            // also ignore exception in Scv semantics
            warn(s"encountered error $e")
            mzero
