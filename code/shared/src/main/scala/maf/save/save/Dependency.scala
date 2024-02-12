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

/**
 * Trait to encode address dependencies.
 *
 * This is an implementation of [[SaveDependency]].
 *
 * @tparam Expr
 *   The type of expression used in the analysis
 */
trait SaveAddrDep[Expr <: Expression] extends SaveDependency[Expr] with SavePosition[Expr] with SaveAddr[Expr]:
    override protected def encodeDependency(writer: Writer, dependency: Dependency)(using AbstractEncoder): Writer =
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
trait SaveDependency[Expr <: Expression] extends SaveMapToArray with SaveComponentID[Expr]:
    /**
     * Get the encoder that will be used to encode dependencies.
     *
     * This will influence how dependencies will be encoded, this can be e.g. a [[MapEncoder map-based]] encoder or an [[ArrayEncoder array-based]]
     * encoder.
     */
    def getDependencyEncoder: AbstractEncoder = getEncoder
    override def saveInfo: Map[String, Savable[_]] =
        super.saveInfo + ("dependencies" -> Savable(deps))

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
    protected def encodeDependency(writer: Writer, dependency: Dependency)(using AbstractEncoder): Writer =
        System.err.nn.println("The dependency with type `" + dependency.getClass + "` could not be encoded")
        writer

    given EncapsulatedEncoder[Dependency] with
        override val encoder: AbstractEncoder = getDependencyEncoder
        override protected def writeEncapsulated(writer: Writer, value: Dependency): Writer = encodeDependency(writer, value)

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
    /**
     * Get the encoder that will be used to encode addresses.
     *
     * This will influence how addresses will be encoded, this can be e.g. a [[MapEncoder map-based]] encoder or an [[ArrayEncoder array-based]]
     * encoder.
     */
    def getAddressEncoder: AbstractEncoder = getEncoder

    /**
     * Get the encoder that will be used to encode addresses.
     *
     * This encoder is used to encode objects where the key is important, when you e.g. encode a type in the key, some encoders might remove this key,
     * and should therefore not be used here.
     *
     * This will influence how addresses will be encoded, this can be e.g. a [[MapEncoder map-based]] encoder or an [[ArrayEncoder array-based]]
     * encoder.
     */
    def getAddressKeyEncoder: AbstractEncoder = getKeyEncoder

    /**
     * Encodes an address.
     *
     * This method allows for expanding the addresses that can be encoded by overriding it, and allowing you to add new addresses by simply mixin in
     * another trait that overrides this method. If you want to add a new encodable address you can override this method like this:
     * {{{
     * override def encodeAddress(writer: Writer, address: Address)(using AbstractEncoder): Writer =
     *     address match {
     *         case < Address >(...) => < encode address >
     *         case _               => super.encodeDependency(writer, address)
     *     }
     * }}}
     * This is just an example and the actual implementation can also be done differently.
     *
     * @note
     *   This method should not be called directly, but should instead only be called from an encoder.
     *
     * @param writer
     *   The writer to write to
     * @param address
     *   The address to encode
     * @param encoder
     *   Implicit argument that encodes the address
     * @return
     *   The used writer
     */
    protected def encodeAddress(writer: Writer, address: Address)(using encoder: AbstractEncoder): Writer =
        System.err.nn.println("The address with type `" + address.getClass + "` could not be encoded")
        writer

    given EncapsulatedEncoder[Address] with
        override val encoder = getAddressKeyEncoder
        override def writeEncapsulated(writer: Writer, value: Address): Writer = encodeAddress(writer, value)

/**
 * Trait to encode scheme addresses.
 *
 * This is an implementation of [[SaveAddr]].
 */
trait SaveSchemeAddr extends SaveAddr[SchemeExp] with SaveComponentID[SchemeExp] with SaveContext[SchemeExp] with SaveStandardSchemeComponents:
    given EncapsulatedEncoder[VarAddr[EncodeContext]] with
        override val encoder = getAddressEncoder
        override def writeEncapsulated(writer: Writer, address: VarAddr[EncodeContext]): Writer =
            writer.writeMember("id", address.id)
            if address.ctx.asInstanceOf[Option[EncodeContext]].isDefined then
                writer.writeMember("context", address.ctx.asInstanceOf[Option[EncodeContext]].get)
            writer

    given EncapsulatedEncoder[ReturnAddr[EncodeContext]] with
        override val encoder = getAddressEncoder
        override def writeEncapsulated(writer: Writer, address: ReturnAddr[EncodeContext]): Writer =
            writer.writeMember("component", address.cmp.asInstanceOf[Component])(using componentIDEncoder, encoder)
            writer.writeMember("identity", address.idn)

    given EncapsulatedEncoder[PtrAddr[EncodeContext]] with
        override val encoder = getAddressEncoder
        override def writeEncapsulated(writer: Writer, address: PtrAddr[EncodeContext]): Writer =
            writer.writeMember("expression", address.exp.asInstanceOf[SchemeExp])
            if address.ctx.asInstanceOf[Option[EncodeContext]].isDefined then
                writer.writeMember("context", address.ctx.asInstanceOf[Option[EncodeContext]].get)
            writer

    override protected def encodeAddress(writer: Writer, address: Address)(using encoder: AbstractEncoder): Writer =
        address match {
            case varAddr @ VarAddr(_, _) =>
                writer.writeMember("varAddr", varAddr.asInstanceOf[VarAddr[EncodeContext]])
            case returnAddr @ ReturnAddr(_, _) =>
                writer.writeMember("returnAddr", returnAddr.asInstanceOf[ReturnAddr[EncodeContext]])
            case PrmAddr(nam) =>
                writer.writeMember("prmAddr", nam)
            case ptrAddr @ PtrAddr(_, _) =>
                writer.writeMember("ptrAddr", ptrAddr.asInstanceOf[PtrAddr[EncodeContext]])
            case _ => super.encodeAddress(writer, address)
        }
