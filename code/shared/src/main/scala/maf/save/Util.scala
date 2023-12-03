package maf.save

import io.bullet.borer.Encoder
import io.bullet.borer.Writer
import EncapsulatedEncoder.*

trait SaveMapToArray:
    given mapKeyEncoder[K, V](using keyEncoder: Encoder[K], valueEncoder: Encoder[V]): ArrayKeyEncoder[Map[K, V]] with
        override def writeEncapsulated(writer: Writer, map: Map[K, V]): Writer =
            for (key, value) <- map do writer.writeMember(key, value)(using keyEncoder, valueEncoder, this)
            writer
