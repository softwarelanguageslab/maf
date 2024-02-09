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
            reader.readMembers(dependencyDecoders.toArray).value.get.get

trait LoadAddr[Expr <: Expression] extends Load[Expr] with LoadPosition[Expr]:
    def getAddressDecoder: AbstractDecoder = getDecoder
    def getAddressKeyDecoder: AbstractDecoder = getKeyDecoder
    def addressDecoders = List[(String, Decoder[_ <: Address])]()

    given EncapsulatedDecoder[Address] with
        override def decoder: AbstractDecoder = getAddressKeyDecoder
        override protected def readEncapsulated(reader: Reader)(using AbstractDecoder): Address =
            reader.readMembers(addressDecoders.toArray).value.get.get

trait LoadSchemeAddr extends LoadAddr[SchemeExp] with LoadContext[SchemeExp] with LoadComponentID[SchemeExp]:
    given Decoder[SchemeValue] = AbstractDecoder.deriveDecoder(getAddressDecoder)
    given Decoder[maf.language.sexp.Value] = AbstractDecoder.deriveAllDecoders(getAddressDecoder)
    override def addressDecoders =
        super.addressDecoders ++ List(
          ("varAddr", summon[Decoder[VarAddr[Context]]]),
          ("prmAddr", summon[Decoder[PrmAddr]]),
          ("returnAddr", summon[Decoder[ReturnAddr[Component]]]),
          ("ptrAddr", summon[Decoder[PtrAddr[Context]]])
        )

    given EncapsulatedDecoder[ReturnAddr[Component]] with
        override def decoder: AbstractDecoder = getAddressDecoder
        override protected def readEncapsulated(reader: Reader)(using AbstractDecoder): ReturnAddr[Component] =
            val component = reader.readMember[Component]("component")
            val identity = reader.readMember[Identity]("identity")
            return new ReturnAddr[Component](component.value.get.get, identity.value.get.get)

    given EncapsulatedDecoder[VarAddr[Context]] with
        override def decoder: AbstractDecoder = getAddressDecoder
        override protected def readEncapsulated(reader: Reader)(using AbstractDecoder): VarAddr[Context] =
            val name = reader.readMember[Identifier]("id")
            val context = reader.readMember[Context]("context")
            return new VarAddr[Context](name.value.get.get,
                                        if context.isCompleted then Some(context.value.get.get).asInstanceOf[Context] else None.asInstanceOf[Context]
            )

    given Decoder[PrmAddr] with
        override def read(reader: Reader): PrmAddr = new PrmAddr(reader.read[String]())

    given EncapsulatedDecoder[PtrAddr[Context]] with
        override def decoder: AbstractDecoder = getAddressDecoder
        override protected def readEncapsulated(reader: Reader)(using AbstractDecoder): PtrAddr[Context] =
            val expression = reader.readMember[SchemeValue]("expression")
            val context = reader.readMember[Context]("context")
            return new PtrAddr[Context](expression.value.get.get,
                                        if context.isCompleted then Some(context.value.get.get).asInstanceOf[Context] else None.asInstanceOf[Context]
            )
