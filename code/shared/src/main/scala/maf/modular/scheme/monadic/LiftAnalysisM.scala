package maf.modular.scheme.monadic

import maf.modular.scheme.SchemeDomain
import maf.modular.scheme.modflocal.*

/** Implements the analysisM trait for any monad transformer that implements MonadLift, and whose inner monad implements the analyisM monad */
trait LiftAnalysisM extends SchemeSemantics, SchemeDomain, SchemeModFLocalSensitivity
