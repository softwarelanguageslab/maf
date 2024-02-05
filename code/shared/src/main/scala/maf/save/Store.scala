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

trait SaveValue[Expr <: Expression] extends Save[Expr] with AbstractDomain[Expr]:
    def valueEncoder[T]: AbstractEncoder[T] = encoder
    given valueEncoder: Encoder[Value]

trait SaveModularSchemeLattices extends SaveModularDomain with SaveAddr[SchemeExp] with SaveStandardSchemeComponents:
    type SchemeLattice = ModularSchemeLattice[?, ?, ?, ?, ?, ?, ?]
    given EncapsulatedEncoder[(HMapKey, SchemeLattice#Value)] with
        override val encoder = valueEncoder[(HMapKey, SchemeLattice#Value)]
        override protected def writeEncapsulated(writer: Writer, hMapPair: (HMapKey, SchemeLattice#Value)): Writer =
            val (key, value) = hMapPair
            def writeValue[T: Encoder](value: T) = writer.writeMember("value", value)
            def openValueArray(amount: Int) = writer.write("value").writeArrayOpen(amount)

            writer.writeMember("type", value.typeName)
            value match {
                case int: SchemeLattice#Int   => writeValue(int.i.toString())
                case bool: SchemeLattice#Bool => writeValue(bool.b.toString())
                case str: SchemeLattice#Str   => writeValue(str.s.toString())
                case prim: SchemeLattice#Prim => writeValue(prim.prims)
                case clo: SchemeLattice#Clo =>
                    writer.open("value", 2)
                    clo.closures.foreach((clo) =>
                        writer.writeMember("expression", clo._1)
                        writer.writeMember("address", clo._2.toString())
                    )
                    writer.close()
                case pointer: SchemeLattice#Pointer =>
                    openValueArray(pointer.ptrs.size)
                    pointer.ptrs.foreach(writer.write(_))
                    writer.writeArrayClose()
                case _ =>
                    System.err.nn.println("The lattice with type `" + key.getClass + "` could not be encoded")
                    writer
            }

    override def encodeHMapPair(writer: Writer, key: HMapKey, value: Any)(using encoder: AbstractEncoder[_]): Writer =
        if value.isInstanceOf[SchemeLattice#Value] then writer.writeMember((key, value.asInstanceOf[SchemeLattice#Value]))
        else return super.encodeHMapPair(writer, key, value)

trait SaveModularDomain extends SaveValue[SchemeExp] with ModularSchemeDomain:
    def hMapEncoder[T]: AbstractEncoder[T] = new ArrayEncoder[T]
    def encodeHMapPair(writer: Writer, key: HMapKey, value: Any)(using encoder: AbstractEncoder[_]): Writer =
        System.err.nn.println("The lattice with type `" + key.getClass + "` could not be encoded")
        writer

    override given valueEncoder: EncapsulatedEncoder[HMap] with
        override val encoder = hMapEncoder[HMap]
        override def writeEncapsulated(writer: Writer, hmap: HMap): Writer =
            hmap.contents.foreach((key, value) => encodeHMapPair(writer, key, value))
            writer

trait SaveGlobalStore[Expr <: Expression] extends SaveValue[Expr] with SaveAddr[Expr] with SaveMapToArray with GlobalStore[Expr]:
    override def saveInfo: Map[String, Savable[_]] =
        super.saveInfo + ("store" -> Savable(store))
