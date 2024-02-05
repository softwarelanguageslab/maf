package maf.save

import io.bullet.borer.Encoder
import io.bullet.borer.Writer
import io.bullet.borer.derivation.MapBasedCodecs
import io.bullet.borer.derivation.ArrayBasedCodecs

trait AbstractEncoder[T]:
    val mapBasedEncoder: Boolean
    val writeOpenKey = true
    def writeKey(writer: Writer): Writer
    def writeKey[T: Encoder](writer: Writer, key: T): Writer
    def writeValue[T: Encoder](writer: Writer, value: T): Writer
    def openEncapsulation(writer: Writer): Writer
    def openEncapsulation(writer: Writer, amount: Int): Writer
    def closeEncapsulation(writer: Writer): Writer

object AbstractEncoder:
    inline def deriveEncoder[T](encoder: AbstractEncoder[_]): Encoder[T] =
        if encoder.mapBasedEncoder then MapBasedCodecs.deriveEncoder[T] else ArrayBasedCodecs.deriveEncoder[T]
    inline def deriveAllEncoders[T](encoder: AbstractEncoder[_]): Encoder[T] =
        if encoder.mapBasedEncoder then MapBasedCodecs.deriveAllEncoders[T] else ArrayBasedCodecs.deriveAllEncoders[T]

trait EncapsulatedEncoder[T] extends Encoder[T]:
    val encoder: AbstractEncoder[T]
    protected given AbstractEncoder[T] = encoder
    override def write(writer: Writer, value: T): Writer =
        encoder.openEncapsulation(writer)
        writeEncapsulated(writer, value)
        encoder.closeEncapsulation(writer)
    protected def writeEncapsulated(writer: Writer, value: T): Writer

object EncapsulatedEncoder:
    extension (writer: Writer)
        def close()(using encoder: AbstractEncoder[_]): Writer = encoder.closeEncapsulation(writer)
        def writeMember[T: Encoder, U: Encoder](key: T, value: U)(using encoder: AbstractEncoder[_]): Writer =
            encoder.writeKey(writer, key)
            encoder.writeValue(writer, value)
        def writeMember[T: Encoder](value: T)(using encoder: AbstractEncoder[_]): Writer =
            encoder.writeKey(writer)
            encoder.writeValue(writer, value)
        def open()(using encoder: AbstractEncoder[_]): Writer =
            if encoder.writeOpenKey then encoder.writeKey(writer)
            encoder.openEncapsulation(writer)
        def open[T: Encoder](key: T)(using encoder: AbstractEncoder[_]): Writer =
            encoder.writeKey(writer, key)
            encoder.openEncapsulation(writer)
        def open(amount: Int)(using encoder: AbstractEncoder[_]): Writer =
            if encoder.writeOpenKey then encoder.writeKey(writer)
            encoder.openEncapsulation(writer, amount)
        def open[T: Encoder](key: T, amount: Int)(using encoder: AbstractEncoder[_]): Writer =
            encoder.writeKey(writer, key)
            encoder.openEncapsulation(writer)

class MapEncoder[T] extends AbstractEncoder[T]:
    val mapBasedEncoder = true
    private var id = -1
    override def writeKey(writer: Writer): Writer =
        id += 1
        writeKey(writer, id.toString())
    override def writeKey[T: Encoder](writer: Writer, key: T): Writer = writer.write(key)
    override def writeValue[T: Encoder](writer: Writer, value: T): Writer = writer.write(value)
    override def openEncapsulation(writer: Writer): Writer = writer.writeMapStart()
    override def openEncapsulation(writer: Writer, amount: Int): Writer = writer.writeMapOpen(amount)
    override def closeEncapsulation(writer: Writer): Writer = writer.writeMapClose()

class ArrayEncoder[T] extends AbstractEncoder[T]:
    val mapBasedEncoder = false
    override def writeKey(writer: Writer): Writer = writer
    override def writeKey[T: Encoder](writer: Writer, key: T): Writer = writer
    override def writeValue[T: Encoder](writer: Writer, value: T): Writer = writer.write(value)
    override def openEncapsulation(writer: Writer): Writer = writer.writeArrayStart()
    override def openEncapsulation(writer: Writer, amount: Int): Writer = writer.writeArrayOpen(amount)
    override def closeEncapsulation(writer: Writer): Writer = writer.writeArrayClose()

class ArrayKeyEncoder[T] extends ArrayEncoder[T]:
    private var id = -1
    override val writeOpenKey: Boolean = false
    override def writeKey(writer: Writer): Writer =
        id += 1
        writeKey(writer, id.toString())
    override def writeKey[T: Encoder](writer: Writer, key: T): Writer = openEncapsulation(writer, 2).write(key)
    override def writeValue[T: Encoder](writer: Writer, value: T): Writer = closeEncapsulation(writer.write(value))
