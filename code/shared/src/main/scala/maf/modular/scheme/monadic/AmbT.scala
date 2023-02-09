package maf.modular.scheme.monadic

import maf.core.Monad
import maf.core.Monad.*
import maf.modular.scheme.*
import maf.modular.scheme.modflocal.*
import maf.core.Lattice

trait AmbT extends SchemeSemantics, StubAnalysisM:
    this: SchemeDomain with SchemeModFLocalSensitivity =>

    import maf.core.monad.*
    import maf.core.monad.SetT.given

    type AmbT[M[_]] = [A] =>> SetT[M, A]

    // Adds support fot he Amb monad on the monad transformer stack
    protected given setAnalysisM[M[_]: Monad](using next: AnalysisM[M]): AnalysisM[SetT_[M]] = new AnalysisM[SetT_[M]] {
        private val stub: AnalysisM[SetT_[M]] = stubAnalysisM(using lSetT, ulSetT, next, mSetT)
        export stub.{mbottom => _, mjoin => _, *}

        def mbottom[X]: SetT[M, X] = SetT(Monad[M].unit(Set()))

        def mjoin[X: Lattice](x: SetT_[M][X], y: SetT_[M][X]): SetT_[M][X] =
            SetT(x.runSet.flatMap(xs => y.runSet.map(ys => xs ++ ys)))
    }
