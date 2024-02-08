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

    given EncapsulatedEncoder[VarAddr[Context]] with
        override val encoder = getAddressEncoder
        override def writeEncapsulated(writer: Writer, address: VarAddr[Context]): Writer =
            import componentIDEncoder.given
            writer.writeMember("id", address.id)
            if address.ctx.asInstanceOf[Option[Context]].isDefined then writer.writeMember("context", address.ctx.asInstanceOf[Option[Context]].get)
            writer

    given EncapsulatedEncoder[ReturnAddr[Context]] with
        override val encoder = getAddressEncoder
        override def writeEncapsulated(writer: Writer, address: ReturnAddr[Context]): Writer =
            import componentIDEncoder.given
            writer.writeMember("component", address.cmp.asInstanceOf[Component])
            writer.writeMember("identity", address.idn)

    given EncapsulatedEncoder[PtrAddr[Context]] with
        override val encoder = getAddressEncoder
        override def writeEncapsulated(writer: Writer, address: PtrAddr[Context]): Writer =
            writer.writeMember("expression", address.exp.asInstanceOf[SchemeValue])
            if address.ctx.asInstanceOf[Option[Context]].isDefined then writer.writeMember("context", address.ctx.asInstanceOf[Option[Context]].get)
            writer

    override def encodeAddress(writer: Writer, address: Address)(using encoder: AbstractEncoder): Writer =
        import componentIDEncoder.given
        address match {
            case varAddr @ VarAddr(_, _) =>
                writer.writeMember("varAddr", varAddr.asInstanceOf[VarAddr[Context]])
            case returnAddr @ ReturnAddr(_, _) =>
                writer.writeMember("returnAddr", returnAddr.asInstanceOf[ReturnAddr[Context]])
            case PrmAddr(nam) =>
                writer.writeMember("prmAddr", nam)
            case ptrAddr @ PtrAddr(_, _) =>
                writer.writeMember("ptrAddr", ptrAddr.asInstanceOf[PtrAddr[Context]])
            case _ => super.encodeAddress(writer, address)
        }
