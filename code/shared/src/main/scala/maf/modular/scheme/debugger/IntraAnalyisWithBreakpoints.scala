package maf.modular.scheme.debugger

import maf.modular.scheme.modflocal.SchemeSemantics
import maf.language.scheme.SchemeExp
import maf.modular.scheme.SchemeDomain
import maf.modular.scheme.modflocal.SchemeModFLocalSensitivity
import maf.language.scheme.SchemeVar
import maf.core.Identifier

trait IntraAnalyisWithBreakpoints extends SchemeSemantics:
    this: SchemeDomain with SchemeModFLocalSensitivity =>

    override def eval(exp: SchemeExp): A[Val] = exp match
        case SchemeVar(Identifier("break", _)) => ???
        case _                                 => super.eval(exp)
