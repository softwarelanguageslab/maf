package maf.save

import io.bullet.borer.derivation.MapBasedCodecs
import io.bullet.borer.derivation.ArrayBasedCodecs
import io.bullet.borer.Decoder
import io.bullet.borer.Reader
import scala.collection.mutable.HashMap
import io.bullet.borer.derivation.CompactMapBasedCodecs
import scala.concurrent.Future
import scala.concurrent.Promise
import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.reflect.ClassTag

/**
 * Base trait for an decoder.
 *
 * This trait has methods for reading keys and values with a decoder from a given reader and depending on the implementation off this trait, this can
 * either read your values from a map or an array. This allows you to decode your values in a certain way, but without knowing exactly how it was
 * stored (e.g. in a map or in an array).
 *
 * This trait also has method for opening and closing an encapsulation, this will open/close either a map or an array based on the implementation of
 * this trait.
 *
 * @note
 *   This trait gives the methods needed to read values, but not the implementation. Other traits like [[MapDecoder]] or [[ArrayDecoder]] should be
 *   used depending on how you want your value to be decoded.
 *
 * @note
 *   Some decoders like [[ArrayDecoder]] will also not decode your key, because arrays do not require keys like maps do, if the key does need to be
 *   stored, you should use an decoder that writes them down like [[MapDecoder]] or [[ArrayKeyDecoder]] if you want to use an array.
 */
trait AbstractDecoder[T] extends Decoder[T]:
    /**
     * Read a key from the given reader.
     *
     * @note
     *   This only reads a key and should therefore be followed up by reading a value using [[readValue]].
     *
     * @note
     *   In some decoders like [[ArrayDecoder]] this will not read anything as this decoder does not use keys.
     *
     * @param reader
     *   The reader used read the key
     * @return
     *   The key that was read.
     */
    def readKey(reader: Reader): String

    /**
     * Read a given key from the given reader.
     *
     * This will read a given key, if this key is not yet readable because it only appears later in the file, the reading of this key will be delayed
     * until it becomes available.
     *
     * @note
     *   This only reads a key and should therefore be followed up by reading a value using [[readValue]].
     *
     * @note
     *   In some decoders like [[ArrayDecoder]] this will not read anything as this decoder does not use keys.
     *
     * @param reader
     *   The reader used read the key
     * @param key
     *   The key that should be read
     * @return
     *   The key that was read.
     */
    def readKey(reader: Reader, key: String): String

    /**
     * Read a given value from the given reader.
     *
     * This will read a value of type T using the previously read key, if this key is not yet available because it appears later in the file, this
     * will be delayed until it becomes available. In this case the returned object will not have a value yet, which will be filled in when the key
     * becomes available.
     *
     * @note
     *   This only reads a value and should therefore only be called after reading a key using [[readKey]].
     *
     * @note
     *   In some decoders like [[ArrayDecoder]] the value will always be read immediately because there are no keys.
     *
     * @param reader
     *   The reader used read the value
     * @tparam T
     *   The type of value that should be decoded
     * @returns
     *   The key-value pair that is read, if the key cannot be read yet because it appears later in the file, the value will be empty and will be
     *   filled in later.
     *
     * @throws IllegalStateException
     *   When you call this before calling [[readKey]]
     */
    def readValue[T: Decoder](reader: Reader): T = reader.read[T]()

    /**
     * Opens either a new map or a new array, based on the decoder that is used.
     *
     * @note
     *   This only opens a new encapsulation and should therefore only be called after reading a key using [[readKey]].
     *
     * @param reader
     *   The reader used read the array/map
     */
    def openEncapsulation(reader: Reader): Unit

    /**
     * Opens either a new map or a new array, based on the decoder that is used.
     *
     * @note
     *   This only opens a new encapsulation and should therefore only be called after reading a key using [[readKey]].
     *
     * @param reader
     *   The reader used read the array/map
     * @param amount
     *   The amount of elements the map/array, this is only useful when using CBOR the length of maps and arrays is used in the encoding
     */
    def openEncapsulation(reader: Reader, amount: Int): Unit

    /**
     * Closes the map/array, based on the decoder that is used.
     *
     * @note
     *   This only closes the encapsulation and should therefore only be called after having already opened an encapsulation using
     *   [[openEncapsulation]].
     *
     * @param reader
     *   The reader used read the array/map
     * @param unbounded
     *   Wether the array/map had a fixed length or not
     * @param res
     *   The object that was encoded inside of the array/map
     * @tparam T
     *   The type of the object that was encoded inside of the array/map
     */
    def closeEncapsulation[T](reader: Reader): Unit

    /**
     * Read the first element that you can, discarding any other elements.
     *
     * @param reader
     *   The reader used read
     */
    def forceReadValue(reader: Reader): Unit

    /** [TODO: description] */
    def decodeOption[T: Decoder](reader: Reader, key: String): Option[T]

    extension (reader: Reader)
        /**
         * Read a key-value pair.
         *
         * This will read a key-value pair in either a map or an array, based on the decoder that is given. If the key cannot be read yet because it
         * appears later in the file, the resulting value will not have a value yet, this value will be filled in when the key becomes readable.
         *
         * @param key
         *   The key to read
         * @param decoder
         *   Implicit argument that decides how to read the key-value pair
         * @tparam T
         *   The type of the value that should be read, this type should have an decoder
         * @returns
         *   The key-value pair that is read, if the key cannot be read yet because it appears later in the file, the value will be empty and will be
         *   filled in later.
         */
        def readMember[T: Decoder](key: String): T =
            readKey(reader, key)
            readValue[T](reader)

        /**
         * Read a key-value pair.
         *
         * This will read a key-value pair in either a map or an array, based on the decoder that is given. The key will be the first available key or
         * a autogenerated key if you are using a decoder that doesn't use keys like [[ArrayDecoder]]
         *
         * @param key
         *   The key to read
         * @param decoder
         *   Implicit argument that decides how to read the key-value pair
         * @tparam T
         *   The type of the value that should be read, this type should have an decoder
         * @returns
         *   The key-value pair that is read.
         */
        def readMember[T: Decoder](): (String, T) =
            val key = readKey(reader)
            (key, readValue[T](reader))

        def readOptionMember[T](key: String)(using Decoder[T]): Option[T] =
            return decodeOption[T](reader, key)

        /**
         * Read one of the given key-value pairs.
         *
         * This will read the first key that is found and return the value associated with it, if non of the keys can currently be read, the returned
         * value will not have a key or a value. These will be filled in when a key matching one of the given keys is encountered.
         *
         * @note
         *   When using a decoder that doesn't use keys like [[ArrayDecoder]] the first decoder given will be used.
         *
         * @param keys
         *   The possible keys that could be read
         * @param decoder
         *   Implicit argument that decides how to read the key-value pair
         * @tparam T
         *   The type of the value that should be read, this type should have an decoder
         * @returns
         *   The key-value pair that is read, if none of the keys can be read because it appears later in the file, the key and the value will be
         *   empty and will be filled in later.
         */
        def readMembers[T](keys: Map[String, Decoder[_ <: T]]): (String, T) =
            var key = readKey(reader)
            if !keys.contains(key) then reader.unexpectedDataItem("", key)
            return (key, reader.read[T]()(using keys.get(key).get.asInstanceOf[Decoder[T]]))

        /**
         * Read until a map/array end is found and accumulate the result without reading the actual map/array end.
         *
         * @note
         *   The given function should also read the values, this method will just continuously call the given function with the returned value until
         *   a break is reached.
         *
         * @param zero
         *   The initial alue
         * @param f
         *   The function transforming the previous value into the next value, this should include reading the value
         * @return
         *   The final value
         */
        def readUntilBeforeBreak[T](zero: T, f: T => T): T =
            var res = zero
            while !reader.hasBreak do res = f(res)
            return res

        /** [TODO: description] */
        def start(): Unit =
            openEncapsulation(reader)

        def start(amount: Int): Unit = openEncapsulation(reader, amount)

        def open(): Unit =
            readKey(reader);
            openEncapsulation(reader);

        def open(amount: Int): Unit =
            readKey(reader);
            openEncapsulation(reader, amount);

        /** [TODO: description] */
        def close(): Unit =
            readUntilBeforeBreak(None,
                                 (none: None.type) =>
                                     forceReadValue(reader)
                                     None
            )
            closeEncapsulation(reader)

/**
 * Decoder that uses maps to decode values.
 *
 * This decoder uses maps to decode values and therefore requires keys to be present.
 *
 * @example
 * {{{
 * given MapDecoder[T]
 *     override protected def read(reader: Reader): T =
 *           reader.start()
 *           < decode value >
 *           reader.close()
 * }}}
 *
 * @tparam T
 *   The type to encode
 */
trait MapDecoder[T] extends AbstractDecoder[T]:
    override def readKey(reader: Reader): String = return reader.readString()
    override def readKey(reader: Reader, key: String): String =
        val read = reader.readString()
        if !read.equals(key) then reader.unexpectedDataItem(key, read)
        return read
    override def openEncapsulation(reader: Reader): Unit = reader.readMapStart()
    override def openEncapsulation(reader: Reader, amount: Int): Unit = reader.readMapOpen(amount)
    override def closeEncapsulation[T](reader: Reader): Unit = reader.readBreak()
    override def forceReadValue(reader: Reader): Unit =
        reader.skipElement()
        reader.skipElement()
    override def decodeOption[T: Decoder](reader: Reader, key: String): Option[T] =
        if reader.tryReadString(key) then return Some(reader.read[T]())
        else return None

/**
 * Decoder that uses arrays to decode values.
 *
 * This decoder uses arrays to decode values and does not require keys, if you want an array-based decoder that saves keys, you should use
 * [[ArrayKeyDecoder]].
 *
 * @example
 * {{{
 * given ArrayDecoder[T]
 *     override protected def read(reader: Reader): T =
 *           reader.start()
 *           < decode value >
 *           reader.close()
 * }}}
 *
 * @tparam T
 *   The type to encode
 */
trait ArrayDecoder[T] extends AbstractDecoder[T]:
    /** Used to generate IDs if no key is provided, this is used to store the values. */
    protected var id = -1
    override def readKey(reader: Reader): String = return ""
    override def readKey(reader: Reader, key: String): String = return key
    override def openEncapsulation(reader: Reader): Unit = reader.readArrayStart()
    override def openEncapsulation(reader: Reader, amount: Int): Unit = reader.readArrayOpen(amount)
    override def closeEncapsulation[T](reader: Reader): Unit = reader.readBreak()
    override def decodeOption[T: Decoder](reader: Reader, key: String): Option[T] =
        var res: Option[T] = None
        reader.readArrayOpen(1)
        if !reader.hasBreak then res = Some(reader.read[T]())
        reader.readBreak()
        return res
    override def forceReadValue(reader: Reader): Unit =
        reader.skipElement()

/**
 * Decoder that uses arrays to decode values, but preserves keys.
 *
 * This decoder uses arrays to decode values but requires values to be directly preceded by a key. This can be used, for example, if your keys are not
 * strings. Since non-string keys are not supported in JSON, you cannot use a map for this, but this class does allow you to load your key-value pair
 * in an intuitive way.
 *
 * This is how your key-value pair should be saved:
 * {{{
 * [
 *     < key1 >,
 *     < value1 >,
 *     < key2 >,
 *     < value2> ,
 *     ...
 * ]
 * }}}
 *
 * @example
 * {{{
 * given ArrayKeyDecoder[T]
 *     override protected def read(reader: Reader): T =
 *           reader.start()
 *           < decode value >
 *           reader.close()
 * }}}
 *
 * @tparam T
 *   The type to encode
 */
trait ArrayKeyDecoder[T] extends MapDecoder[T]:
    /** The key that was read, and which value should now be read. */
    protected var key: Option[Any] = None

    /**
     * Read a key from the given reader.
     *
     * @note
     *   This only reads a key and should therefore be followed up by reading a value using [[readValue]].
     *
     * @param reader
     *   The reader used read the key
     * @tparam T
     *   The type of the key that should be read
     * @return
     *   The key that was read.
     */
    def readKey[T: Decoder](reader: Reader): Any =
        key = Some(reader.read[T]())
        key.get

    /**
     * Read a given value from the given reader.
     *
     * This will read a value of type T using the previously read key.
     *
     * @note
     *   This only reads a value and should therefore only be called after reading a key using [[readKey]].
     *
     * @param reader
     *   The reader used read the value
     * @tparam V
     *   The type of key that was decoded by [[readKey]]
     * @tparam T
     *   The type of value that should be decoded
     * @returns
     *   The key-value pair that is read, if the key cannot be read yet because it appears later in the file, the value will be empty and will be
     *   filled in later.
     *
     * @throws IllegalStateException
     *   When you call this before calling [[readKey]]
     */
    def readKeyValue[V, T: Decoder](reader: Reader): (V, T) =
        if key.isEmpty then throw new IllegalStateException(s"Trying to read a value before reading a key.")
        val res = reader.read[T]()
        val tmpKey = key
        key = None
        return (tmpKey.get.asInstanceOf[V], res)
    override def openEncapsulation(reader: Reader): Unit = reader.readArrayStart()
    override def openEncapsulation(reader: Reader, amount: Int): Unit = reader.readArrayOpen(amount)
    override def closeEncapsulation[T](reader: Reader): Unit = reader.readBreak()

    extension (reader: Reader)
        /**
         * Reads a key-value pair.
         *
         * This will read a key-value pair in either a map or an array, based on the decoder that is given.
         *
         * @param decoder
         *   Implicit argument that decides how to read the key-value pair
         * @tparam T
         *   The type of the key that should be read, this type should have an decoder
         * @tparam U
         *   The type of the value that should be read, this type should have an decoder
         */
        def readMember[K: Decoder, V: Decoder](): (K, V) =
            readKey[K](reader)
            readKeyValue[K, V](reader)
