package maf.save

import io.bullet.borer.Encoder
import io.bullet.borer.Writer
import EncapsulatedEncoder.*

trait SaveMapToArray:
    given mapKeyEncoder[K, V](using keyEncoder: Encoder[K], valueEncoder: Encoder[V]): EncapsulatedEncoder[Map[K, V]] with
        override val encoder = new ArrayKeyEncoder
        override def writeEncapsulated(writer: Writer, map: Map[K, V]): Writer =
            for (key, value) <- map do writer.writeMember(key, value)
            writer
