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
    override def loadInfo: List[(String, Loadable[_])] =
        super.loadInfo ++ List(("dependencies", Loadable((deps: Map[Dependency, Set[Component]]) => this.deps = deps)))

    /** Returns a map that links a key to a specific decoder. */
    def dependencyDecoders = Set[(String, Decoder[_ <: Dependency])]()

    given MapDecoder[Dependency] with
        override def read(reader: Reader): Dependency =
            reader.start()
            val dependency = reader.readMembers(dependencyDecoders.toArray).value
            reader.close()
            return dependency

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
    /** Returns a map that links a key to a specific decoder. */
    def addressDecoders = List[(String, Decoder[_ <: Address])]()

    given MapDecoder[Address] with
        override def read(reader: Reader): Address =
            reader.start()
            val address = reader.readMembers(addressDecoders.toArray).value
            reader.close()
            return address

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

    given MapDecoder[ReturnAddr[Component]] with
        override def read(reader: Reader): ReturnAddr[Component] =
            reader.start()
            val component = reader.readMember[Component]("component")
            val identity = reader.readMember[Identity]("identity")
            reader.close()
            return new ReturnAddr[Component](component.value, identity.value)

    given MapDecoder[VarAddr[DecodeContext]] with
        override def read(reader: Reader): VarAddr[DecodeContext] =
            reader.start()
            val name = reader.readMember[Identifier]("id")
            val context = reader.readMember[DecodeContext]("context")
            reader.close()
            return new VarAddr[DecodeContext](
              name.value,
              if context.hasValue then Some(context.value).asInstanceOf[DecodeContext] else None.asInstanceOf[DecodeContext]
            )

    given Decoder[PrmAddr] with
        override def read(reader: Reader): PrmAddr = new PrmAddr(reader.read[String]())

    given MapDecoder[PtrAddr[DecodeContext]] with
        override def read(reader: Reader): PtrAddr[DecodeContext] =
            reader.start()
            val expression = reader.readMember[SchemeExp]("expression")
            val context = reader.readMember[DecodeContext]("context")
            reader.close()
            return new PtrAddr[DecodeContext](
              expression.value,
              if context.hasValue then Some(context.value).asInstanceOf[DecodeContext] else None.asInstanceOf[DecodeContext]
            )
