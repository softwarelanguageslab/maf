package maf.analysis

import maf.values.typeclasses.MonadJoin
import cats.data.OptionT
import cats.extensions.MonadError
import maf.values.Lattice

// type M = (ErrorT (StoT (EnvT (CtxT (AllocT Id)))))

type ErrorT[M[_]] = [A] =>> M[(Set[maf.util.Error], Option[A])]
