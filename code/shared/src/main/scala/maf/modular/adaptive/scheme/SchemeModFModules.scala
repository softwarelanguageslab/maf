package maf.modular.adaptive.scheme

import maf.modular.scheme.modf._
import maf.language.scheme._
import maf.modular.scheme.modf.SchemeModFComponent._
import maf.modular.scheme.ModularSchemeDomain

trait SchemeModFModules extends BaseSchemeModFSemantics with ModularSchemeDomain {

  import modularLatticeWrapper.modularLattice.{schemeLattice => lat}

  trait SchemeModule
  case object MainModule extends SchemeModule {
    override def toString = "main"
  }
  case class LambdaModule(lambda: SchemeLambdaExp) extends SchemeModule {
    override def toString = lambda.lambdaName
    override def hashCode = lambda.idn.hashCode
    override def equals(other: Any) = other match {
      case lm: LambdaModule if lm.lambda.idn == this.lambda.idn => true
      case _                                                    => false
    }
  }

  def module(cmp: Component): SchemeModule = view(cmp) match {
    case Main         => MainModule
    case Call(clo, _) => module(clo)
    case _            => throw new Exception("Should not happen!")
  }

  def module(clo: lat.Closure): LambdaModule = LambdaModule(clo._1)
}
