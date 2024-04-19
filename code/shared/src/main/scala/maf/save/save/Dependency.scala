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
import maf.language.scheme.SchemeValue
import maf.core.Identifier
import maf.util.Writer.write
import maf.save.save.SaveExpressions
import maf.modular.scheme.SchemeAddr

/**
 * Trait to encode address dependencies.
 *
 * This is an implementation of [[SaveDependency]].
 *
 * @tparam Expr
 *   The type of expression used in the analysis
 */
trait SaveAddrDep[Expr <: Expression] extends SaveDependency[Expr] with SavePosition[Expr] with SaveAddr[Expr]:
    override protected def encodeDependency(writer: Writer, dependency: Dependency): Writer =
        dependency match {
            case AddrDependency(addr) => writer.writeMember("addrDependency", addr)
            case _                    => super.encodeDependency(writer, dependency)
        }

/**
 * Base trait for encoding dependencies.
 *
 * @note
 *   This trait gives the methods needed to encode dependencies, but not the implementation. Other traits like [[SaveAddrDep]] should be mixed in. The
 *   exact trait that is mixed in depends on the dependencies that you are using in your analysis.
 *
 * @tparam Expr
 *   The type of expression used in the analysis
 */
trait SaveDependency[Expr <: Expression] extends SaveMapToArray with SaveComponents[Expr]:
    override def saveInfo: List[(String, Savable[_])] =
        super.saveInfo ++ List(("dependencies" -> Savable(deps)))

    /**
     * Encodes a dependency.
     *
     * This method allows for expanding the dependencies that can be encoded by overriding it, and allowing you to add new dependencies by simply
     * mixin in another trait that overrides this method. If you want to add a new encodable dependency you can override this method like this:
     * {{{
     * override def encodeDependency(writer: Writer, dependency: Dependency)(using AbstractEncoder): Writer =
     *     dependency match {
     *         case < Dependency >(...) => < encode dependency >
     *         case _               => super.encodeDependency(writer, dependency)
     *     }
     * }}}
     * This is just an example and the actual implementation can also be done differently.
     *
     * @note
     *   This method should not be called directly, but should instead only be called from an encoder.
     *
     * @param writer
     *   The writer to write to
     * @param dependency
     *   The dependency to encode
     * @param encoder
     *   Implicit argument that encodes the dependency
     * @return
     *   The used writer
     */
    protected def encodeDependency(writer: Writer, dependency: Dependency): Writer =
        System.err.nn.println("The dependency with type `" + dependency.getClass + "` could not be encoded")
        writer

    given MapEncoder[Dependency] with
        override def write(writer: Writer, value: Dependency): Writer =
            writer.start()
            encodeDependency(writer, value)
            writer.close()

/**
 * Base trait for encoding addresses.
 *
 * @note
 *   This trait gives the methods needed to encode addresses, but not the implementation. Other traits like [[SaveSchemeAddr]] should be mixed in. The
 *   exact trait that is mixed in depends on the addresses that you are using in your analysis.
 *
 * @tparam Expr
 *   The type of expression used in the analysis
 */
trait SaveAddr[Expr <: Expression] extends Save[Expr] with SavePosition[Expr]:
    given addressEncoder: Encoder[Address]

/**
 * Trait to encode scheme addresses.
 *
 * This is an implementation of [[SaveAddr]].
 */
trait SaveSchemeAddr extends SaveAddr[SchemeExp] with SaveComponents[SchemeExp] with SaveContext[SchemeExp] with SaveExpressions[SchemeExp]:
    given MapEncoder[VarAddr[EncodeContext]] with
        override def write(writer: Writer, address: VarAddr[EncodeContext]): Writer =
            writer.start()
            writer.writeMember("id", address.id)
            if address.ctx.asInstanceOf[Option[EncodeContext]].isDefined then
                writer.writeMember("context", address.ctx.asInstanceOf[Option[EncodeContext]].get)
            writer.close()

    given MapEncoder[ReturnAddr[Component]] with
        override def write(writer: Writer, address: ReturnAddr[Component]): Writer =
            writer.start()
            writer.writeMember("identity", address.idn)
            writer.writeMember("component", address.cmp)
            writer.close()

    given MapEncoder[PtrAddr[EncodeContext]] with
        override def write(writer: Writer, address: PtrAddr[EncodeContext]): Writer =
            writer.start()
            writer.writeMember("expression", address.exp.asInstanceOf[SchemeExp])
            if address.ctx.asInstanceOf[Option[EncodeContext]].isDefined then
                writer.writeMember("context", address.ctx.asInstanceOf[Option[EncodeContext]].get)
            writer.close()

    override given addressEncoder: MapEncoder[Address] with
        override def write(writer: Writer, address: Address) =
            writer.start()
            address match {
                case varAddr @ VarAddr(_, _) =>
                    writer.writeMember("varAddr", varAddr.asInstanceOf[VarAddr[EncodeContext]])
                case returnAddr @ ReturnAddr(_, _) =>
                    writer.writeMember("returnAddr", returnAddr.asInstanceOf[ReturnAddr[Component]])
                case PrmAddr(nam) =>
                    writer.writeMember("prmAddr", nam)
                case ptrAddr @ PtrAddr(_, _) =>
                    writer.writeMember("ptrAddr", ptrAddr.asInstanceOf[PtrAddr[EncodeContext]])
                case _ =>
                    System.err.nn.println("The scheme address with type `" + address.getClass + "` could not be encoded")
                    writer
            }
            writer.close()
