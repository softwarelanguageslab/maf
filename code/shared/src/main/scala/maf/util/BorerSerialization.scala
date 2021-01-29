package maf.util

import io.bullet.borer._
import io.bullet.borer.derivation.MapBasedCodecs._
import maf.language.scheme._
import maf.core._
import maf.language.sexp.Value
import maf.modular.scheme._
import maf.modular.scheme.modf._
import maf.modular.worklist._
import scala.reflect.ClassTag

object Conversions {
  implicit def setToArray[T: ClassTag](set: Set[T]): Array[T] = set.toArray

  implicit def idn: Codec[Identity.IDN] = deriveCodec
  implicit def identity: Codec[Identity] = deriveAllCodecs
  implicit def identifier: Codec[Identifier] = deriveCodec
  implicit def value: Codec[Value] = deriveAllCodecs
  implicit def schemeLambdaExp: Codec[SchemeLambdaExp] = deriveAllCodecs
  implicit def schemeExp: Codec[SchemeExp] = deriveAllCodecs
}

class SerializableAnalysis(prg: SchemeExp) extends SimpleSchemeModFAnalysis(prg)
    with SchemeConstantPropagationDomain
    with SchemeModFCallSiteSensitivity
    with LIFOWorklistAlgorithm[SchemeExp] {
  import Conversions._
  // TODO: derivation for addresses will be tricky, but let's assume we have it already
  implicit val a: Codec[Addr]
  // TODO: Same for environments (once we have it for addresses, it's only a matter of dealing with the D type parameter of WrappedEnv)
  implicit val env: Codec[Environment[Addr]]

  // ComponentContext = CallSiteContext (in SchemeModFKCallSiteSensitivity)
  implicit val cmpCtx: Codec[ComponentContext] = deriveCodec

  // To emphasize that the error is not caused by a missing Encoder (see error later):
  implicit val cmpCtxEncoder: Encoder[ComponentContext] = deriveEncoder

  // Component = SchemeModFComponent (in StandardSchemeModFComponents)
  implicit val cmp: Codec[Component] = deriveAllCodecs
  // Results in the following error:
  // Error:
  // Could not find implicit Encoder[SchemeModFComponents.this.ComponentContext] for parameter `ctx` of case class SchemeModFComponents.this.Call

}
