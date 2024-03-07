package maf.save

import maf.core.Expression
import maf.modular.GlobalStore
import io.bullet.borer.Encoder
import maf.modular.AbstractDomain
import maf.language.scheme.SchemeExp
import maf.modular.scheme.ModularSchemeDomain
import maf.lattice.HMap
import io.bullet.borer.Writer
import maf.lattice.HMapKey
import maf.language.scheme.lattices.ModularSchemeLattice
import scala.reflect.ClassTag
import maf.lattice.AbstractWrapType
import maf.lattice.AbstractType
import maf.lattice.AbstractSetType
import io.bullet.borer.LowPrioEncoders
import maf.core.Address
import EncapsulatedEncoder.*
import maf.core.Environment
import maf.lattice.{ConcreteLattice, ConstantPropagation}
import maf.lattice.Concrete

/**
 * Base trait for encoding [[AbstractDomain.Value values]].
 *
 * @note
 *   This trait gives the methods needed to encode values, but not the implementation. Other traits like [[SaveModularDomain]] should be mixed in. The
 *   exact trait that is mixed in depends on the values that you are using in your analysis.
 *
 * @tparam Expr
 *   The type of expression used in the analysis
 */
trait SaveValue[Expr <: Expression] extends Save[Expr] with AbstractDomain[Expr]:
    /**
     * Get the encoder that will be used to encode values.
     *
     * This will influence how values will be encoded, this can be e.g. a [[maf.save.MapEncoder map-based]] encoder or an [[ArrayEncoder array-based]]
     * encoder.
     */
    def getValueEncoder: AbstractEncoder = getEncoder

    /**
     * Get the encoder that will be used to encode values.
     *
     * This encoder is used to encode objects where the key is important, when you e.g. encode a type in the key, some encoders might remove this key,
     * and should therefore not be used here.
     *
     * This will influence how values will be encoded, this can be e.g. a [[MapEncoder map-based]] encoder or an [[ArrayEncoder array-based]] encoder.
     */
    def getValueKeyEncoder: AbstractEncoder = getKeyEncoder

    /** Encodes a value */
    given valueEncoder: Encoder[Value]

/**
 * Trait to encode lattices.
 *
 * @tparam Expr
 *   The type of expression used in the analysis
 */
trait SaveLattice[Expr <: Expression] extends Save[Expr]:
    /**
     * Get the encoder that will be used to encode lattices.
     *
     * This will influence how lattices will be encoded, this can be e.g. a [[MapEncoder map-based]] encoder or an [[ArrayEncoder array-based]]
     * encoder.
     */
    def getLatticeEncoder: AbstractEncoder = getEncoder

    /**
     * Get the encoder that will be used to encode lattices.
     *
     * This encoder is used to encode objects where the key is important, when you e.g. encode a type in the key, some encoders might remove this key,
     * and should therefore not be used here.
     *
     * This will influence how lattices will be encoded, this can be e.g. a [[MapEncoder map-based]] encoder or an [[ArrayEncoder array-based]]
     * encoder.
     */
    def getLatticeKeyEncoder: AbstractEncoder = getKeyEncoder

    /**
     * The types of lattices that can be encoded by this trait.
     *
     * This is used to specify the givens, if this was not used, this given could be used for every class with a single abstract type.
     */
    type Lattice[T] = ConstantPropagation.L[T] | Concrete.L[T]
    given latticeEncoder[P[T] <: Lattice[T], T: Encoder]: EncapsulatedEncoder[P[T]] with
        override val encoder: AbstractEncoder = getLatticeKeyEncoder
        override protected def writeEncapsulated(writer: Writer, lattice: P[T]): Writer =
            lattice match
                case constant: ConstantPropagation.L[T] =>
                    writer.writeMember("constant", constant)(using constantLatticeEncoder, encoder)
                case _ => System.err.nn.println("The lattice of type `" + lattice.getClass + "` could not be encoded")
            writer

    /** Encodes [[ConstantPropagation.L constant lattices]]. */
    given constantLatticeEncoder[T: Encoder]: Encoder[ConstantPropagation.L[T]] with
        override def write(writer: Writer, lattice: ConstantPropagation.L[T]): Writer =
            lattice match
                case ConstantPropagation.Top         => writer.write("top")
                case ConstantPropagation.Constant(a) => writer.write[T](a)
                case ConstantPropagation.Bottom      => writer.write("bottom")
            writer

/**
 * Trait to encode [[ModularSchemeLattice modular scheme lattices]].
 *
 * Implementation of [[SaveModularDomain]]
 */
trait SaveModularSchemeLattices
    extends SaveModularDomain
    with SaveAddr[SchemeExp]
    with SaveStandardSchemeComponents
    with SaveEnvironment[SchemeExp]
    with SaveLattice[SchemeExp]:
    /** Generic modular scheme lattice that is used for typechecking of nested class inside of this. */
    type SchemeLattice = ModularSchemeLattice[?, ?, ?, ?, ?, ?, ?]

    given EncapsulatedArrayEncoder[SchemeLattice#Clo]() with
        override val encoder = getValueEncoder
        override protected def writeEncapsulated(writer: Writer, closure: SchemeLattice#Clo): Writer =
            closure.closures.foreach((clo) =>
                encoder.openEncapsulation(writer)
                writer.writeMember("expression", clo._1)
                writer.writeMember("address", clo._2.asInstanceOf[Environment[Address]])
                encoder.closeEncapsulation(writer)
            )
            writer

    given EncapsulatedArrayEncoder[SchemeLattice#Pointer]() with
        override val encoder = getValueEncoder
        override protected def writeEncapsulated(writer: Writer, pointer: SchemeLattice#Pointer): Writer =
            pointer.ptrs.foreach(writer.write(_))
            writer

    given EncapsulatedEncoder[SchemeLattice#Cons]() with
        override val encoder = getValueEncoder
        override protected def writeEncapsulated(writer: Writer, cons: SchemeLattice#Cons): Writer =
            writer.writeMember("car", cons.car)
            writer.writeMember("cdr", cons.cdr)

    given EncapsulatedEncoder[(HMapKey, SchemeLattice#Value)] with
        override val encoder = getValueKeyEncoder
        override protected def writeEncapsulated(writer: Writer, hMapPair: (HMapKey, SchemeLattice#Value)): Writer =
            val (key, value) = hMapPair

            value match {
                case int: SchemeLattice#Int         => writer.writeMember("int", int.i.asInstanceOf[Lattice[BigInt]])
                case bool: SchemeLattice#Bool       => writer.writeMember("boolean", bool.b.asInstanceOf[Lattice[Boolean]])
                case str: SchemeLattice#Str         => writer.writeMember("string", str.s.asInstanceOf[Lattice[String]])
                case symbol: SchemeLattice#Symbol   => writer.writeMember("symbol", symbol.s.asInstanceOf[Lattice[String]])
                case prim: SchemeLattice#Prim       => writer.writeMember("primitive", prim.prims)
                case clo: SchemeLattice#Clo         => writer.writeMember("closure", clo)
                case pointer: SchemeLattice#Pointer => writer.writeMember("pointer", pointer)
                case cons: SchemeLattice#Cons       => writer.writeMember("cons", cons)
                case modularLattice.Nil             => writer.writeMember("nil", "")
                case modularLattice.Void            => writer.writeMember("void", "")
                case _ =>
                    System.err.nn.println("The lattice with type `" + key.getClass + "` could not be encoded")
                    writer
            }

    override def encodeHMapPair(writer: Writer, key: HMapKey, value: Any)(using encoder: AbstractEncoder): Writer =
        if value.isInstanceOf[SchemeLattice#Value] then writer.writeMember((key, value.asInstanceOf[SchemeLattice#Value]))
        else return super.encodeHMapPair(writer, key, value)

/**
 * Base trait for encoding values as [[ModularSchemeLattice modular scheme lattices]], as defined in [[ModularSchemeDomain]].
 *
 * Implementation of [[SaveValue]].
 *
 * @note
 *   This trait gives the methods needed to encode values, but not the implementation. Other traits like [[SaveModularSchemeLattices]] should be mixed
 *   in. The exact trait that is mixed in depends on the values that you are using in your analysis.
 */
trait SaveModularDomain extends SaveValue[SchemeExp] with ModularSchemeDomain:
    /**
     * Get the encoder that will be used to encode an hMap.
     *
     * This will influence how an hMap will be encoded, this can be e.g. a [[MapEncoder map-based]] encoder or an [[ArrayEncoder array-based]]
     * encoder.
     *
     * @note
     *   This is an [[ArrayEncoder array encoder]] by default
     */
    def getHMapEncoder: AbstractEncoder = new ArrayEncoder

    /**
     * Encodes an hMap pair.
     *
     * This method allows for expanding the hMap pairs that can be encoded by overriding it, and allowing you to add new hMap pairs by simply mixin in
     * another trait that overrides this method. If you want to add a new encodable hMap pair you can override this method like this:
     * {{{
     * override def encodeHMapPair(writer: Writer, key: HMapKey, value: Any)(using AbstractEncoder): Writer =
     *     address match {
     *         case < ... >(...) => < encode hMap pair >
     *         case _               => super.encodeHMapPair(writer, key, value)
     *     }
     * }}}
     * This is just an example and the actual implementation can also be done differently.
     *
     * @note
     *   This method should not be called directly, but should instead only be called from an encoder.
     *
     * @param writer
     *   The writer to write to
     * @param key
     *   The key used in the hMap
     * @param value
     *   The value that is associated to the key
     * @param encoder
     *   Implicit argument that encodes the hMap pair
     * @return
     *   The used writer
     */
    def encodeHMapPair(writer: Writer, key: HMapKey, value: Any)(using encoder: AbstractEncoder): Writer =
        System.err.nn.println("The lattice with type `" + key.getClass + "` could not be encoded")
        writer

    override given valueEncoder: EncapsulatedEncoder[HMap] with
        override val encoder = getHMapEncoder
        override def writeEncapsulated(writer: Writer, hmap: HMap): Writer =
            hmap.contents.foreach((key, value) => encodeHMapPair(writer, key, value))
            writer

/**
 * Trait to encode the global store.
 *
 * This adds the global store to the objects that should be saved, but does not have an implementation that can be used to encode the
 * [[AbstractDomain.Value values]] inside of the store, for this an implementation of [[SaveValue]] like [[SaveModularDomain]] should be included
 * depending on the values that are used in your analysis.
 */
trait SaveGlobalStore[Expr <: Expression] extends SaveValue[Expr] with SaveAddr[Expr] with SaveMapToArray with GlobalStore[Expr]:
    override def saveInfo: List[(String, Savable[_])] =
        return super.saveInfo ++ List(("store" -> Savable(store)))
