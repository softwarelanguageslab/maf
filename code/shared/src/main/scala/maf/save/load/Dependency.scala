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

trait LoadAddr[Expr <: Expression] extends Load[Expr] with LoadPosition[Expr]:
    def getAddressDecoder: AbstractDecoder = getDecoder
    def addressDecoders = List[(String, Decoder[_ <: Address])]()

    given EncapsulatedDecoder[Address] with
        override def decoder: AbstractDecoder = getAddressDecoder
        override protected def readEncapsulated(reader: Reader)(using AbstractDecoder): Address =
            reader.readMembers(addressDecoders.toArray).value.get.get

trait LoadSchemeAddr extends LoadAddr[SchemeExp] with LoadContext[SchemeExp]:
    given Decoder[SchemeValue] = AbstractDecoder.deriveDecoder(getAddressDecoder)
    given Decoder[maf.language.sexp.Value] = AbstractDecoder.deriveAllDecoders(getAddressDecoder)
    override def addressDecoders =
        super.addressDecoders ++ List(("varAddr", summon[Decoder[VarAddr[Context]]]),
                                      ("prmAddr", summon[Decoder[PrmAddr]]),
                                      ("ptrAddr", summon[Decoder[PtrAddr[Context]]])
        )

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
