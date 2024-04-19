package maf.save

import io.bullet.borer.Encoder
import io.bullet.borer.Writer

/**
 * Trait to encode a map using an array.
 *
 * This will save your map in an array with alternating keys and values:
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
trait SaveMapToArray:
    /**
     * Encodes a map using an array.
     *
     * This will save your map in an array with alternating keys and values:
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
    given mapKeyEncoder[K, V](using keyEncoder: Encoder[K], valueEncoder: Encoder[V]): ArrayKeyEncoder[Map[K, V]] with
        override def write(writer: Writer, map: Map[K, V]): Writer =
            writer.start()
            for (key, value) <- map do writer.writeMember(key, value)(using keyEncoder, valueEncoder)
            writer.close()
