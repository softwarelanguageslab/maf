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
import maf.core.Environment
import maf.lattice.{ConcreteLattice, ConstantPropagation}
import maf.lattice.Concrete
import maf.save.save.SaveExpressions

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
     * The types of lattices that can be encoded by this trait.
     *
     * This is used to specify the givens, if this was not used, this given could be used for every class with a single abstract type.
     */
    type Lattice[T] = ConstantPropagation.L[T] | Concrete.L[T]
    given latticeEncoder[P[T] <: Lattice[T], T: Encoder]: MapEncoder[P[T]] with
        override def write(writer: Writer, lattice: P[T]): Writer =
            writer.start()
            lattice match
                case constant: ConstantPropagation.L[T] =>
                    writer.writeMember("constant", constant)(using constantLatticeEncoder)
                case _ => System.err.nn.println("The lattice of type `" + lattice.getClass + "` could not be encoded")
            writer.close()

    /** Encodes [[ConstantPropagation.L constant lattices]]. */
    given constantLatticeEncoder[T: Encoder]: Encoder[ConstantPropagation.L[T]] with
        override def write(writer: Writer, lattice: ConstantPropagation.L[T]): Writer =
            lattice match
                case ConstantPropagation.Top         => writer.write("top")
                case ConstantPropagation.Constant(a) => writer.write[T](a)
                case ConstantPropagation.Bottom      => writer.write("bottom")
            writer

/**
 * Trait for encoding values as [[ModularSchemeLattice modular scheme lattices]], as defined in [[ModularSchemeDomain]].
 *
 * Implementation of [[SaveValue]].
 */
trait SaveModularDomain
    extends SaveValue[SchemeExp]
    with ModularSchemeDomain
    with SaveAddr[SchemeExp]
    with SaveExpressions[SchemeExp]
    with SaveEnvironment[SchemeExp]
    with SaveLattice[SchemeExp]:
    /** Generic modular scheme lattice that is used for typechecking of nested class inside of this. */
    type SchemeLattice = ModularSchemeLattice[?, ?, ?, ?, ?, ?, ?]
    override given valueEncoder: ArrayEncoder[HMap] with
        override def write(writer: Writer, hmap: HMap): Writer =
            writer.start()
            hmap.contents.foreach((key, value) => writer.writeMember((key, value)))
            writer.close()

    given MapEncoder[SchemeLattice#Clo]() with
        override def write(writer: Writer, closure: SchemeLattice#Clo): Writer =
            writer.writeArrayOpen(closure.closures.size)
            closure.closures.foreach((clo) =>
                writer.start()
                writer.writeMember("expression", clo._1.asInstanceOf[SchemeExp])
                writer.writeMember("address", clo._2.asInstanceOf[Environment[Address]])
                writer.close()
            )
            writer.writeArrayClose()

    given ArrayEncoder[SchemeLattice#Pointer]() with
        override def write(writer: Writer, pointer: SchemeLattice#Pointer): Writer =
            writer.start()
            pointer.ptrs.foreach(writer.write(_))
            writer.close()

    given MapEncoder[SchemeLattice#Cons]() with
        override def write(writer: Writer, cons: SchemeLattice#Cons): Writer =
            writer.start()
            writer.writeMember("car", cons.car)
            writer.writeMember("cdr", cons.cdr)
            writer.close()

    given MapEncoder[SchemeLattice#Vec]() with SaveMapToArray with
        override def write(writer: Writer, vec: SchemeLattice#Vec): Writer =
            writer.start()
            writer.writeMember("size", vec.size.asInstanceOf[Lattice[BigInt]])
            writer.writeMember("elements", vec.elements.asInstanceOf[Map[Lattice[BigInt], SchemeLattice#L]])
            writer.close()

    given MapEncoder[(HMapKey, Any)] with
        override def write(writer: Writer, hMapPair: (HMapKey, Any)): Writer =
            writer.start()
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
                case vec: SchemeLattice#Vec         => writer.writeMember("vector", vec)
                case modularLattice.Nil             => writer.writeMember("nil", "")
                case modularLattice.Void            => writer.writeMember("void", "")
                case _ =>
                    System.err.nn.println("The lattice with type `" + key.getClass + "` could not be encoded")
                    writer.writeMember("ERROR", "Unknown type: " + key.getClass.toString())
            }
            writer.close()

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
