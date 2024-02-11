package maf.save

import maf.language.scheme.SchemeExp
import maf.modular.Dependency
import maf.modular.AddrDependency
import maf.core.Expression
import maf.core.Address
import maf.modular.scheme.VarAddr
import maf.modular.ReturnAddr
import maf.modular.scheme.PrmAddr
import maf.modular.scheme.PtrAddr
import maf.language.scheme.SchemeValue
import io.bullet.borer.Reader
import io.bullet.borer.Decoder
import maf.save.EncapsulatedDecoder.*
import maf.core.Identifier
import maf.core.Identity

trait LoadAddrDependency[Expr <: Expression] extends LoadDependency[Expr] with LoadPosition[Expr] with LoadAddr[Expr]:
    given Decoder[AddrDependency] with
        override def read(reader: Reader): AddrDependency =
            val addr = reader.read[Address]()
            return new AddrDependency(addr)
    override def dependencyDecoders = super.dependencyDecoders ++ Set(("addrDependency", summon[Decoder[AddrDependency]]))

trait LoadDependency[Expr <: Expression] extends LoadMapToArray with LoadComponentID[Expr]:
    def getDependencyDecoder: AbstractDecoder = getDecoder
    def getDependencyKeyDecoder: AbstractDecoder = getKeyDecoder
    override def loadInfo: Map[String, Loadable[_]] =
        super.loadInfo + ("dependencies" -> Loadable((deps: Map[Dependency, Set[Component]]) => this.deps = deps))
    def dependencyDecoders = Set[(String, Decoder[_ <: Dependency])]()

    given EncapsulatedDecoder[Dependency] with
        override def decoder: AbstractDecoder = getDependencyDecoder
        override protected def readEncapsulated(reader: Reader)(using AbstractDecoder): Dependency =
            return reader.readMembers(dependencyDecoders.toArray).value

trait LoadAddr[Expr <: Expression] extends Load[Expr] with LoadPosition[Expr]:
    def getAddressDecoder: AbstractDecoder = getDecoder
    def getAddressKeyDecoder: AbstractDecoder = getKeyDecoder
    def addressDecoders = List[(String, Decoder[_ <: Address])]()

    given EncapsulatedDecoder[Address] with
        override def decoder: AbstractDecoder = getAddressKeyDecoder
        override protected def readEncapsulated(reader: Reader)(using AbstractDecoder): Address =
            reader.readMembers(addressDecoders.toArray).value

trait LoadSchemeAddr extends LoadAddr[SchemeExp] with LoadContext[SchemeExp] with LoadComponentID[SchemeExp]:
    given Decoder[SchemeValue] = AbstractDecoder.deriveDecoder(getAddressDecoder)
    given Decoder[maf.language.sexp.Value] = AbstractDecoder.deriveAllDecoders(getAddressDecoder)
    override def addressDecoders =
        super.addressDecoders ++ List(
          ("varAddr", summon[Decoder[VarAddr[DecodeContext]]]),
          ("prmAddr", summon[Decoder[PrmAddr]]),
          ("returnAddr", summon[Decoder[ReturnAddr[Component]]]),
          ("ptrAddr", summon[Decoder[PtrAddr[DecodeContext]]])
        )

    given EncapsulatedDecoder[ReturnAddr[Component]] with
        override def decoder: AbstractDecoder = getAddressDecoder
        override protected def readEncapsulated(reader: Reader)(using AbstractDecoder): ReturnAddr[Component] =
            val component = reader.readMember[Component]("component")
            val identity = reader.readMember[Identity]("identity")
            return new ReturnAddr[Component](component.value, identity.value)

    given EncapsulatedDecoder[VarAddr[DecodeContext]] with
        override def decoder: AbstractDecoder = getAddressDecoder
        override protected def readEncapsulated(reader: Reader)(using AbstractDecoder): VarAddr[DecodeContext] =
            val name = reader.readMember[Identifier]("id")
            val context = reader.readMember[DecodeContext]("context")
            return new VarAddr[DecodeContext](
              name.value,
              if context.hasValue then Some(context.value).asInstanceOf[DecodeContext] else None.asInstanceOf[DecodeContext]
            )

    given Decoder[PrmAddr] with
        override def read(reader: Reader): PrmAddr = new PrmAddr(reader.read[String]())

    given EncapsulatedDecoder[PtrAddr[DecodeContext]] with
        override def decoder: AbstractDecoder = getAddressDecoder
        override protected def readEncapsulated(reader: Reader)(using AbstractDecoder): PtrAddr[DecodeContext] =
            val expression = reader.readMember[SchemeValue]("expression")
            val context = reader.readMember[DecodeContext]("context")
            return new PtrAddr[DecodeContext](
              expression.value,
              if context.hasValue then Some(context.value).asInstanceOf[DecodeContext] else None.asInstanceOf[DecodeContext]
            )
