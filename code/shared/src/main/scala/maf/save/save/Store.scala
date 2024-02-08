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

trait SaveValue[Expr <: Expression] extends Save[Expr] with AbstractDomain[Expr]:
    def getValueEncoder: AbstractEncoder = getEncoder
    given valueEncoder: Encoder[Value]

trait SaveModularSchemeLattices extends SaveModularDomain with SaveAddr[SchemeExp] with SaveStandardSchemeComponents with SaveEnvironment[SchemeExp]:
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

    given EncapsulatedEncoder[(HMapKey, SchemeLattice#Value)] with
        override val encoder = getValueEncoder
        override protected def writeEncapsulated(writer: Writer, hMapPair: (HMapKey, SchemeLattice#Value)): Writer =
            val (key, value) = hMapPair

            value match {
                case int: SchemeLattice#Int         => writer.writeMember("int", int.i.toString())
                case bool: SchemeLattice#Bool       => writer.writeMember("boolean", bool.b.toString())
                case str: SchemeLattice#Str         => writer.writeMember("string", str.s.toString())
                case prim: SchemeLattice#Prim       => writer.writeMember("primitive", prim.prims)
                case clo: SchemeLattice#Clo         => writer.writeMember("closure", clo)
                case pointer: SchemeLattice#Pointer => writer.writeMember("pointer", pointer)
                case _ =>
                    System.err.nn.println("The lattice with type `" + key.getClass + "` could not be encoded")
                    writer
            }

    override def encodeHMapPair(writer: Writer, key: HMapKey, value: Any)(using encoder: AbstractEncoder): Writer =
        if value.isInstanceOf[SchemeLattice#Value] then writer.writeMember((key, value.asInstanceOf[SchemeLattice#Value]))
        else return super.encodeHMapPair(writer, key, value)

trait SaveModularDomain extends SaveValue[SchemeExp] with ModularSchemeDomain:
    def getHMapEncoder: AbstractEncoder = new ArrayEncoder
    def encodeHMapPair(writer: Writer, key: HMapKey, value: Any)(using encoder: AbstractEncoder): Writer =
        System.err.nn.println("The lattice with type `" + key.getClass + "` could not be encoded")
        writer

    override given valueEncoder: EncapsulatedEncoder[HMap] with
        override val encoder = getHMapEncoder
        override def writeEncapsulated(writer: Writer, hmap: HMap): Writer =
            hmap.contents.foreach((key, value) => encodeHMapPair(writer, key, value))
            writer

trait SaveGlobalStore[Expr <: Expression] extends SaveValue[Expr] with SaveAddr[Expr] with SaveMapToArray with GlobalStore[Expr]:
    override def saveInfo: Map[String, Savable[_]] =
        super.saveInfo + ("store" -> Savable(store))
