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
        override def writeEncapsulated(writer: Writer, value: Address): Writer =
            writer.writeMember("position", value.idn.pos)
            encodeAddress(writer, value)

trait SaveSchemeAddr extends SaveAddr[SchemeExp] with SaveStandardSchemeComponentID:
    override def encodeAddress(writer: Writer, address: Address)(using encoder: AbstractEncoder): Writer =
        import componentIDEncoder.given
        address match {
            case VarAddr(id, ctx) =>
                writer.writeMember("type", "varAddr")
                writer.writeMember("name", id.name)
            case ReturnAddr(cmp, idn) =>
                writer.writeMember("type", "returnAddr")
                writer.writeMember("component", cmp.asInstanceOf[Component])
            case PrmAddr(nam) =>
                writer.writeMember("name", nam)
                writer.writeMember("type", "prmAddr")
            case PtrAddr(exp, ctx) =>
                writer.writeMember("type", "ptrAddr")
            case _ => super.encodeAddress(writer, address)
        }
