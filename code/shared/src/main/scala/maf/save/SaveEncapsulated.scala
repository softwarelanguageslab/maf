package maf.save

import io.bullet.borer.Encoder
import io.bullet.borer.Writer

trait EncapsulatedEncoder[T] extends Encoder[T]:
    override def write(writer: Writer, value: T): Writer = writeEncapsulated(writer, value)
    def writeEncapsulated(writer: Writer, value: T): Writer
    def writeMembe[T: Encoder, U: Encoder](writer: Writer, key: T, value: U): Writer = writer.writeMember(key, value)
    extension (writer: Writer)
        def writeMember[T: Encoder, U: Encoder](key: T, value: U): Writer
        def writeMember[T: Encoder](value: T): Writer

trait MapEncoder[T] extends EncapsulatedEncoder[T]:
    final override def write(writer: Writer, value: T): Writer =
        writer.writeMapStart()
        super.write(writer, value)
        writer.writeMapClose()

    private var id = -1
    extension (writer: Writer)
        override def writeMember[A: Encoder, B: Encoder](key: A, value: B): Writer =
            writer.writeMapMember(key, value)

        override def writeMember[T: Encoder](value: T): Writer =
            id += 1
            writer.writeMember(id.toString(), value)

trait ArrayEncoder[T] extends EncapsulatedEncoder[T]:
    final override def write(writer: Writer, value: T): Writer =
        writer.writeArrayStart()
        super.write(writer, value)
        writer.writeArrayClose()

    extension (writer: Writer)
        override def writeMember[A: Encoder, B: Encoder](key: A, value: B): Writer =
            writer.write(value)

        override def writeMember[T: Encoder](value: T): Writer =
            writer.write(value)

trait ArrayKeyEncoder[T] extends ArrayEncoder[T]:
    extension (writer: Writer)
        override def writeMember[A: Encoder, B: Encoder](key: A, value: B): Writer =
            writer.writeArrayOpen(2)
            writer.write(key).write(value)
            writer.writeArrayClose()
