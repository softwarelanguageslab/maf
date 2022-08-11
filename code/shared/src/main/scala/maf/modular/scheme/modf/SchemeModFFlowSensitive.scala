package maf.modular.scheme.modf

import maf.modular.scheme.modf.BigStepModFSemanticsT
import maf.core.Address
import maf.core.Lattice

trait TEvalMFlowSensitive[M[_], V: Lattice] extends TEvalM[M]:
    def write(a: Address, v: V): M[Unit]
    def read(a: Address): M[V]

trait SchemeModFFlowSensitive extends BigStepModFSemanticsT:
    type EvalM[_] <: TEvalMFlowSensitive[_, Value]
