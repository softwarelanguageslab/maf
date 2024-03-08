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

/**
 * Trait to decode address dependencies.
 *
 * Implementation of [[LoadDependency]].
 *
 * @tparam Expr
 *   The type of expression used in the analysis
 */
trait LoadAddrDependency[Expr <: Expression] extends LoadDependency[Expr] with LoadPosition[Expr] with LoadAddr[Expr]:
    given Decoder[AddrDependency] with
        override def read(reader: Reader): AddrDependency =
            val addr = reader.read[Address]()
            return new AddrDependency(addr)
    override def dependencyDecoders = super.dependencyDecoders ++ Set(("addrDependency", summon[Decoder[AddrDependency]]))

/**
 * The base trait for decoding dependencies.
 *
 * @note
 *   This trait gives the methods needed to decode dependencies, but does not implement them yet, other traits like [[LoadAddrDependency]] should be
 *   mixed in for the implementation. The trait that should be mixed in depends on the kind of dependencies that is used in your analysis.
 *
 * @tparam Expr
 *   The type of expression used in the analysis
 */
trait LoadDependency[Expr <: Expression] extends LoadMapToArray with LoadComponents[Expr]:
    /**
     * Get the decoder that will be used to decode dependencies.
     *
     * This will influence how dependencies will be decoded, this can be e.g. a [[MapDecoder map-based]] decoder or an [[ArrayDecoder array-based]]
     * decoder.
     */
    def getDependencyDecoder: AbstractDecoder = getDecoder

    /**
     * Get the decoder that will be used to decode dependencies.
     *
     * This decoder is used to decode objects where the key is important, when you want to e.g. decode a type from the key, some decoders might ignore
     * this key, and should therefore not be used here.
     *
     * This will influence how dependencies will be decoded, this can be e.g. a [[MapDecoder map-based]] decoder or an [[ArrayDecoder array-based]]
     * decoder.
     */
    def getDependencyKeyDecoder: AbstractDecoder = getKeyDecoder
    override def loadInfo: List[(String, Loadable[_])] =
        super.loadInfo ++ List(("dependencies", Loadable((deps: Map[Dependency, Set[Component]]) => this.deps = deps)))

    /** Returns a map that links a key to a specific decoder. */
    def dependencyDecoders = Set[(String, Decoder[_ <: Dependency])]()

    given EncapsulatedDecoder[Dependency] with
        override def decoder: AbstractDecoder = getDependencyDecoder
        override protected def readEncapsulated(reader: Reader)(using AbstractDecoder): Dependency =
            return reader.readMembers(dependencyDecoders.toArray).value

/**
 * The base trait for decoding addresses.
 *
 * @note
 *   This trait gives the methods needed to decode addresses, but does not implement them yet, other traits like [[LoadAddrDependency]] should be
 *   mixed in for the implementation. The trait that should be mixed in depends on the kind of addresses that is used in your analysis.
 *
 * @tparam Expr
 *   The type of expression used in the analysis
 */
trait LoadAddr[Expr <: Expression] extends Load[Expr] with LoadPosition[Expr]:
    /**
     * Get the decoder that will be used to decode addresses.
     *
     * This will influence how addresses will be decoded, this can be e.g. a [[MapDecoder map-based]] decoder or an [[ArrayDecoder array-based]]
     * decoder.
     */
    def getAddressDecoder: AbstractDecoder = getDecoder

    /**
     * Get the decoder that will be used to decode addresses.
     *
     * This decoder is used to decode objects where the key is important, when you want to e.g. decode a type from the key, some decoders might ignore
     * this key, and should therefore not be used here.
     *
     * This will influence how addresses will be decoded, this can be e.g. a [[MapDecoder map-based]] decoder or an [[ArrayDecoder array-based]]
     * decoder.
     */
    def getAddressKeyDecoder: AbstractDecoder = getKeyDecoder

    /** Returns a map that links a key to a specific decoder. */
    def addressDecoders = List[(String, Decoder[_ <: Address])]()

    given EncapsulatedDecoder[Address] with
        override def decoder: AbstractDecoder = getAddressKeyDecoder
        override protected def readEncapsulated(reader: Reader)(using AbstractDecoder): Address =
            reader.readMembers(addressDecoders.toArray).value

/**
 * Trait to decode scheme addresses.
 *
 * This is an implementation of [[LoadAddr]].
 */
trait LoadSchemeAddr extends LoadAddr[SchemeExp] with LoadContext[SchemeExp] with LoadComponents[SchemeExp] with LoadExpressions[SchemeExp]:
    override def addressDecoders =
        super.addressDecoders ++ List(
          ("varAddr", summon[Decoder[VarAddr[DecodeContext]]]),
          ("prmAddr", summon[Decoder[PrmAddr]]),
          ("returnAddr", summon[Decoder[ReturnAddr[Component]]]),
          ("ptrAddr", summon[Decoder[PtrAddr[DecodeContext]]])
        )

    given EncapsulatedDecoder[ReturnAddr[Component]] with
        override def decoder: AbstractDecoder = getAddressDecoder
        override protected def readEncapsulated(reader: Reader)(using decoder: AbstractDecoder): ReturnAddr[Component] =
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
            val expression = reader.readMember[SchemeExp]("expression")
            val context = reader.readMember[DecodeContext]("context")
            return new PtrAddr[DecodeContext](
              expression.value,
              if context.hasValue then Some(context.value).asInstanceOf[DecodeContext] else None.asInstanceOf[DecodeContext]
            )
