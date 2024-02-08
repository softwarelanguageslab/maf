package maf.save

import io.bullet.borer.Encoder
import io.bullet.borer.Writer
import io.bullet.borer.derivation.MapBasedCodecs
import io.bullet.borer.derivation.ArrayBasedCodecs
import scala.collection.mutable.HashMap
import io.bullet.borer.derivation.CompactMapBasedCodecs
import scala.concurrent.Future
import scala.concurrent.Promise
import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global

trait AbstractEncoder:
    val mapBasedEncoder: Boolean
    val writeOpenKey = true
    def writeKey(writer: Writer): Writer
    def writeKey(writer: Writer, key: String): Writer
    def writeValue[T: Encoder](writer: Writer, value: T): Writer
    def openEncapsulation(writer: Writer): Writer
    def openEncapsulation(writer: Writer, amount: Int): Writer
    def closeEncapsulation(writer: Writer): Writer

object AbstractEncoder:
    inline def deriveEncoder[T](encoder: AbstractEncoder): Encoder[T] =
        if encoder.mapBasedEncoder then CompactMapBasedCodecs.deriveEncoder[T] else ArrayBasedCodecs.deriveEncoder[T]
    inline def deriveAllEncoders[T](encoder: AbstractEncoder): Encoder[T] =
        if encoder.mapBasedEncoder then CompactMapBasedCodecs.deriveAllEncoders[T] else ArrayBasedCodecs.deriveAllEncoders[T]

trait EncapsulatedEncoder[T] extends Encoder[T]:
    val encoder: AbstractEncoder
    protected given AbstractEncoder = encoder
    override def write(writer: Writer, value: T): Writer =
        encoder.openEncapsulation(writer)
        writeEncapsulated(writer, value)
        encoder.closeEncapsulation(writer)
    protected def writeEncapsulated(writer: Writer, value: T): Writer

trait EncapsulatedArrayEncoder[T](length: Int = 0) extends EncapsulatedEncoder[T]:
    override def write(writer: Writer, value: T): Writer =
        if length == 0 then writer.writeArrayStart() else writer.writeArrayOpen(length)
        writeEncapsulated(writer, value)
        writer.writeArrayClose()

object EncapsulatedEncoder:
    extension (writer: Writer)
        def close()(using encoder: AbstractEncoder): Writer = encoder.closeEncapsulation(writer)
        def writeMember[T: Encoder](key: String, value: T)(using encoder: AbstractEncoder): Writer =
            encoder.writeKey(writer, key)
            encoder.writeValue(writer, value)
        def writeMember[T: Encoder](value: T)(using encoder: AbstractEncoder): Writer =
            encoder.writeKey(writer)
            encoder.writeValue(writer, value)
        def open()(using encoder: AbstractEncoder): Writer =
            if encoder.writeOpenKey then encoder.writeKey(writer)
            encoder.openEncapsulation(writer)
        def open(key: String)(using encoder: AbstractEncoder): Writer =
            encoder.writeKey(writer, key)
            encoder.openEncapsulation(writer)
        def open(amount: Int)(using encoder: AbstractEncoder): Writer =
            if encoder.writeOpenKey then encoder.writeKey(writer)
            encoder.openEncapsulation(writer, amount)
        def open(key: String, amount: Int)(using encoder: AbstractEncoder): Writer =
            encoder.writeKey(writer, key)
            encoder.openEncapsulation(writer)

class MapEncoder extends AbstractEncoder:
    val mapBasedEncoder = true
    private var id = -1
    override def writeKey(writer: Writer): Writer =
        id += 1
        writeKey(writer, id.toString())
    override def writeKey(writer: Writer, key: String): Writer = writer.write(key)
    override def writeValue[T: Encoder](writer: Writer, value: T): Writer = writer.write(value)
    override def openEncapsulation(writer: Writer): Writer = writer.writeMapStart()
    override def openEncapsulation(writer: Writer, amount: Int): Writer = writer.writeMapOpen(amount)
    override def closeEncapsulation(writer: Writer): Writer = writer.writeMapClose()

class ArrayEncoder extends AbstractEncoder:
    val mapBasedEncoder = false
    override def writeKey(writer: Writer): Writer = writer
    override def writeKey(writer: Writer, key: String): Writer = writer
    override def writeValue[T: Encoder](writer: Writer, value: T): Writer = writer.write(value)
    override def openEncapsulation(writer: Writer): Writer = writer.writeArrayStart()
    override def openEncapsulation(writer: Writer, amount: Int): Writer = writer.writeArrayOpen(amount)
    override def closeEncapsulation(writer: Writer): Writer = writer.writeArrayClose()

class ArrayKeyEncoder extends ArrayEncoder:
    override val writeOpenKey: Boolean = false
    override def writeKey(writer: Writer, key: String): Writer = writer.write(key)
    def writeKey[T: Encoder](writer: Writer, key: T): Writer = writer.write(key)
    override def writeValue[T: Encoder](writer: Writer, value: T): Writer = writer.write(value)

object ArrayKeyEncoder:
    extension (writer: Writer)
        def writeMember[T: Encoder, U: Encoder](key: T, value: U)(using encoder: ArrayKeyEncoder): Writer =
            encoder.writeKey(writer, key)
            encoder.writeValue(writer, value)
