package maf.save

import maf.core.Expression
import maf.modular.AbstractDomain

trait LoadValue[Expr <: Expression] extends Save[Expr] with AbstractDomain[Expr]
