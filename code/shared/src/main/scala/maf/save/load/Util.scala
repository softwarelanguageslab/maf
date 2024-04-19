package maf.save

import io.bullet.borer.Decoder
import io.bullet.borer.Reader
import scala.collection.mutable

/**
 * Trait to decode a map using an array.
 *
 * This will load a map from an array with alternating keys and values:
 * {{{
 *   [
 *     < key1 >,
 *     < value1 >,
 *     < key2 >,
 *     < value2 >,
 *     ...
 *   ]
 * }}}
 * This can, for example be used if the key is not a string, and can therefore not be used as a key of a JSON map.
 */
trait LoadMapToArray:
    /**
     * Decodes a map using an array.
     *
     * This will load a map from an array with alternating keys and values:
     * {{{
     *   [
     *     < key1 >,
     *     < value1 >,
     *     < key2 >,
     *     < value2 >,
     *     ...
     *   ]
     * }}}
     * This can, for example be used if the key is not a string, and can therefore not be used as a key of a JSON map.
     */
    given mapKeyDecoder[K, V](using keyDecoder: Decoder[K], valueDecoder: Decoder[V]): ArrayKeyDecoder[Map[K, V]] with
        override def read(reader: Reader): Map[K, V] =
            reader.start()
            val elements = mutable.Set[(K, V)]()
            while !reader.hasBreak do
                val res = reader.readMember[K, V]()(using keyDecoder, valueDecoder)
                elements.add(res.key, res.value)
            reader.close()
            return elements.toMap
