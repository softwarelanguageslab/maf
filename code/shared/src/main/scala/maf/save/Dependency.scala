package maf.save

import maf.language.scheme.SchemeExp
import maf.modular.Dependency
import io.bullet.borer.Writer
import maf.modular.AddrDependency
import maf.core.Expression
import maf.core.Address
import maf.modular.scheme.VarAddr
import maf.modular.ReturnAddr
import io.bullet.borer.Encoder
import maf.modular.scheme.PrmAddr
import maf.modular.scheme.PtrAddr
import EncapsulatedEncoder.*
import maf.language.scheme.SchemeValue
import io.bullet.borer.Reader
import io.bullet.borer.Decoder
import maf.save.EncapsulatedDecoder.*
import maf.core.Identifier
import maf.util.Writer.write

trait SaveAddrDep extends SaveDependency with SavePosition[SchemeExp] with SaveSchemeAddr:
    override def encodeDependency(writer: Writer, dependency: Dependency): Writer =
        dependency match {
            case AddrDependency(_) => writer.write(dependency.asInstanceOf[AddrDependency].addr)
            case _                 => super.encodeDependency(writer, dependency)
        }

trait SaveDependency extends SaveMapToArray with SaveStandardSchemeComponentID:
    def getDependencyEncoder: AbstractEncoder = getEncoder
    override def saveInfo: Map[String, Savable[_]] =
        import componentIDEncoder.given
        super.saveInfo + ("dependencies" -> Savable(deps))

    def encodeDependency(writer: Writer, dependency: Dependency): Writer =
        System.err.nn.println("The dependency with type `" + dependency.getClass + "` could not be encoded")
        writer
    given dependencyEncoder: Encoder[Dependency] = encodeDependency _

trait SaveAddr[Expr <: Expression] extends Save[Expr] with SavePosition[Expr]:
    def getAddressEncoder: AbstractEncoder = getEncoder
    def encodeAddress(writer: Writer, address: Address)(using encoder: AbstractEncoder): Writer =
        System.err.nn.println("The address with type `" + address.getClass + "` could not be encoded")
        writer

    given EncapsulatedEncoder[Address] with
        override val encoder = getAddressEncoder
        override def writeEncapsulated(writer: Writer, value: Address): Writer = encodeAddress(writer, value)

trait SaveSchemeAddr extends SaveAddr[SchemeExp] with SaveStandardSchemeComponentID with SaveContext[SchemeExp]:
    given Encoder[SchemeValue] = AbstractEncoder.deriveEncoder(getAddressEncoder)
    given Encoder[maf.language.sexp.Value] = AbstractEncoder.deriveAllEncoders(getAddressEncoder)
    override def encodeAddress(writer: Writer, address: Address)(using encoder: AbstractEncoder): Writer =
        import componentIDEncoder.given
        address match {
            case VarAddr(id, ctx) =>
                writer.open("varAddr")
                writer.writeMember("id", id)
                if ctx.asInstanceOf[Option[Context]].isDefined then writer.writeMember("context", ctx.asInstanceOf[Option[Context]].get)
                writer.close()
            case ReturnAddr(cmp, idn) =>
                writer.open("returnAddr")
                writer.writeMember("component", cmp.asInstanceOf[Component])
                writer.writeMember("identity", idn)
                writer.close()
            case PrmAddr(nam) =>
                writer.writeMember("prmAddr", nam)
            case PtrAddr(exp, ctx) =>
                writer.open("ptrAddr")
                writer.writeMember("expression", exp.asInstanceOf[SchemeValue])
                if ctx.asInstanceOf[Option[Context]].isDefined then writer.writeMember("context", ctx.asInstanceOf[Option[Context]].get)
                writer.close()
            case _ => super.encodeAddress(writer, address)
        }

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
