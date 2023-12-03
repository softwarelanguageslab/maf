package maf.save

import maf.core.Expression
import maf.modular.GlobalStore
import io.bullet.borer.Encoder
import maf.modular.AbstractDomain
import maf.language.scheme.SchemeExp
import maf.modular.scheme.ModularSchemeDomain
import maf.lattice.HMap
import io.bullet.borer.Writer
import EncapsulatedEncoder.*

trait SaveValue[Expr <: Expression] extends Save[Expr] with AbstractDomain[Expr]:
    given valueEncoder: Encoder[Value]

trait SaveModularDomain extends SaveValue[SchemeExp] with ModularSchemeDomain:
    override given valueEncoder: ArrayEncoder[HMap] with
        override def writeEncapsulated(writer: Writer, value: HMap): Writer =
            value.contents.foreach((key, value) => writer.writeMember(value.toString()))
            writer
