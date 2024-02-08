package maf.save

import io.bullet.borer.Decoder
import io.bullet.borer.Reader
import maf.save.ArrayKeyDecoder.readMember
import scala.collection.mutable

trait LoadMapToArray:
    given mapKeyDecoder[K, V](using keyDecoder: Decoder[K], valueDecoder: Decoder[V]): EncapsulatedDecoder[Map[K, V]] with
        override val decoder: ArrayKeyDecoder = new ArrayKeyDecoder
        override protected def readEncapsulated(reader: Reader)(using AbstractDecoder): Map[K, V] =
            val elements = mutable.Set[(K, V)]()
            while !reader.hasBreak do elements.add(reader.readMember[K, V]()(using keyDecoder, valueDecoder, decoder).value.get.get)
            return elements.toMap
