package maf.save

import io.bullet.borer.Encoder
import io.bullet.borer.Writer

trait EncapsulatedEncoder[T] extends Encoder[T]:
    override def write(writer: Writer, value: T): Writer = writeEncapsulated(writer, value)
    protected val writeOpenKey = true
    protected def writeEncapsulated(writer: Writer, value: T): Writer
    protected def writeKey(writer: Writer): Writer
    protected def writeKey[T: Encoder](writer: Writer, key: T): Writer
    protected def writeValue[T: Encoder](writer: Writer, value: T): Writer
    protected def openEncapsulation(writer: Writer): Writer
    protected def openEncapsulation(writer: Writer, amount: Int): Writer
    protected def closeEncapsulation(writer: Writer): Writer

object EncapsulatedEncoder:
    extension (writer: Writer)
        def close()(using encoder: EncapsulatedEncoder[_]): Writer = encoder.closeEncapsulation(writer)
        def writeMember[T: Encoder, U: Encoder](key: T, value: U)(using encoder: EncapsulatedEncoder[_]): Writer =
            encoder.writeKey(writer, key)
            encoder.writeValue(writer, value)
        def writeMember[T: Encoder](value: T)(using encoder: EncapsulatedEncoder[_]): Writer =
            encoder.writeKey(writer)
            encoder.writeValue(writer, value)
        def open()(using encoder: EncapsulatedEncoder[_]): Writer =
            if encoder.writeOpenKey then encoder.writeKey(writer)
            encoder.openEncapsulation(writer)
        def open(amount: Int)(using encoder: EncapsulatedEncoder[_]): Writer =
            if encoder.writeOpenKey then encoder.writeKey(writer)
            encoder.openEncapsulation(writer, amount)

trait MapEncapsulatedEncoder[T] extends EncapsulatedEncoder[T]:
    final override def write(writer: Writer, value: T): Writer =
        writer.writeMapStart()
        super.write(writer, value)
        writer.writeMapClose()

trait MapMemberEncoder[T] extends EncapsulatedEncoder[T]:
    private var id = -1
    override protected def writeKey(writer: Writer): Writer =
        id += 1
        writeKey(writer, id.toString())
    override protected def writeKey[T: Encoder](writer: Writer, key: T): Writer = writer.write(key)
    override protected def writeValue[T: Encoder](writer: Writer, value: T): Writer = writer.write(value)
    override protected def openEncapsulation(writer: Writer): Writer = writer.writeMapStart()
    override protected def openEncapsulation(writer: Writer, amount: Int): Writer = writer.writeMapOpen(amount)
    override protected def closeEncapsulation(writer: Writer): Writer = writer.writeMapClose()
trait MapEncoder[T] extends MapEncapsulatedEncoder[T] with MapMemberEncoder[T]

trait ArrayEncapsulatedEncoder[T] extends EncapsulatedEncoder[T]:
    final override def write(writer: Writer, value: T): Writer =
        writer.writeArrayStart()
        super.write(writer, value)
        writer.writeArrayClose()

trait ArrayMemberEncoder[T] extends EncapsulatedEncoder[T]:
    override protected def writeKey(writer: Writer): Writer = writer
    override protected def writeKey[T: Encoder](writer: Writer, key: T): Writer = writer
    override protected def writeValue[T: Encoder](writer: Writer, value: T): Writer = writer.write(value)
    override protected def openEncapsulation(writer: Writer): Writer = writer.writeArrayStart()
    override protected def openEncapsulation(writer: Writer, amount: Int): Writer = writer.writeArrayOpen(amount)
    override protected def closeEncapsulation(writer: Writer): Writer = writer.writeArrayClose()
trait ArrayEncoder[T] extends ArrayEncapsulatedEncoder[T] with ArrayMemberEncoder[T]

trait ArrayKeyEncoder[T] extends ArrayEncoder[T]:
    private var id = -1
    override protected val writeOpenKey: Boolean = false
    override protected def writeKey(writer: Writer): Writer =
        id += 1
        writeKey(writer, id.toString())
    override protected def writeKey[T: Encoder](writer: Writer, key: T): Writer = openEncapsulation(writer, 2).write(key)
    override protected def writeValue[T: Encoder](writer: Writer, value: T): Writer = closeEncapsulation(writer.write(value))
