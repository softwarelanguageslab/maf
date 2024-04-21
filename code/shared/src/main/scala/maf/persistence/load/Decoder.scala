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
 * A class representing a decoded key-value pair.
 *
 * This is a class that holds a key-value pair after decoding it, but because it it possible that the key that is should be decoded hasn't been
 * encountered yet, it is possible that either the key or the value don't have a value yet. To check if they have a value you can use [[hasKey]] and
 * [[hasValue]], if you try to retrieve the key or the value without one being present, an error will be thrown.
 *
 * @constructor
 *   Create a new key-value pair that doesn't have a key nor a value yet.
 *
 * @tparam K
 *   The type of the key
 * @tparam V
 *   The type of the value
 */
class ReadValue[K, V](private val forceRead: (key: Option[K]) => Unit):
    protected var _key: Option[K] = None
    protected var _value: Option[V] = None
    protected var _updateValue: Option[ReadValue[K, V]] = None
    protected var _forceRead: Option[(key: Option[K]) => Unit] = Some(forceRead)

    /**
     * Create a new key-value pair with a key and a value.
     *
     * @param _key
     *   The key of the key-value pair
     * @param _value
     *   The value of the key-value pair
     */
    def this(_key: K, _value: V) =
        this((key: Option[K]) => ())
        this._key = Some(_key)
        this._value = Some(_value)

    /**
     * Create a new key-value pair with a key.
     *
     * @param _key
     *   The key of the key-value pair
     */
    def this(_key: K, forceRead: (key: Option[K]) => Unit) =
        this(forceRead)
        this._key = Some(_key)

    /** Check if this has a value. */
    def hasValue: Boolean = _value.isDefined

    /**
     * Returns the value, or tries to find if if there is none yet.
     *
     * @note
     *   If there is no value yet, it will first try to find it anyway, discarding any other values that haven't been read yet. If a value can still
     *   not be found, an error will be thrown.
     *
     * @throws NoSuchElementException
     *   If there is no value
     */
    def value: V =
        if _value.isEmpty && _forceRead.isDefined then
            if _key.isDefined then _forceRead.get(Some(_key.get))
            else _forceRead.get(None)
        if _value.isEmpty then
            if _key.isDefined then throw new NoSuchElementException(s"The key '${key}' does not have a value.")
            else throw new NoSuchElementException("This element does not have a value.")
        _value.get

    /** Set the value. */
    def value_=(newValue: V): Unit =
        _value = Some(newValue)
        if _updateValue.isDefined then _updateValue.get.value = newValue

    /** Check if this has a key. */
    def hasKey: Boolean = _key.isDefined

    /**
     * Returns the key, or tries to find it if there is no key yet.
     *
     * @note
     *   If there is no key yet, it will first try to find it anyway, discarding any other key-value pairs that haven't been read yet. If a key can
     *   still not be found, an error will be thrown.
     *
     * @throws NoSuchElementException
     *   If there is no key
     */
    def key: K =
        if _key.isEmpty && _forceRead.isDefined then _forceRead.get(None)
        if _key.isEmpty then throw new NoSuchElementException("This element does not have a key.")
        _key.get

    /** Set the key. */
    def key_=(newKey: K): Unit =
        _key = Some(newKey)
        if _updateValue.isDefined then _updateValue.get.key = newKey

    /** Set the [[ReadValue]] that should have be updated if this [[ReadValue]] gets a key or a value */
    def updateValue_=(newUpdateValue: ReadValue[K, V]) = _updateValue = Some(newUpdateValue)

    /** The [[ReadValue]] that will be updated when this [[ReadValue]] gets a key or a value. */
    def updateValue = _updateValue

class ReadOptionValue[K, V](private val forceRead: (key: Option[K]) => Unit) extends ReadValue[K, Option[V]](forceRead):
    /**
     * Create a new key-value pair with a key and a value.
     *
     * @param _key
     *   The key of the key-value pair
     * @param _value
     *   The value of the key-value pair
     */
    def this(_key: K, _value: Option[V]) =
        this((key: Option[K]) => ())
        this._key = Some(_key)
        this._value = Some(_value)

    /**
     * Create a new key-value pair with a key.
     *
     * @param _key
     *   The key of the key-value pair
     */
    def this(_key: K, forceRead: (key: Option[K]) => Unit) =
        this(forceRead)
        this._key = Some(_key)

    override def value: Option[V] =
        if _value.isEmpty then return None
        return _value.get
    override def hasValue: Boolean = true

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
    def readValue[T: Decoder](reader: Reader): ReadValue[String, T]

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
     * Read a key-value pair.
     *
     * This will read until it finds the specified key, discarding any other elements that it cannot decode.
     *
     * @param reader
     *   The reader used read
     * @param key
     *   The key to find
     */
    def forceReadValue(reader: Reader, key: String): Unit

    /**
     * Read the first element that you can, discarding any other elements.
     *
     * @param reader
     *   The reader used read
     */
    def forceReadValue(reader: Reader): Unit

    /** [TODO: description] */
    def decodeOption[T: Decoder](reader: Reader): Option[T]

    given [T: Decoder]: Decoder[Option[T]] with
        override def read(reader: Reader): Option[T] =
            return decodeOption[T](reader)

    def forceRead(reader: Reader, key: Option[String]) =
        if key.isDefined then forceReadValue(reader, key.get)
        else forceReadValue(reader)
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
        def readMember[T: Decoder](key: String): ReadValue[String, T] =
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
        def readMember[T: Decoder](): ReadValue[String, T] =
            readKey(reader)
            readValue[T](reader)

        def readOptionMember[T](key: String)(using Decoder[T]): ReadValue[String, Option[T]] =
            readKey(reader, key)
            val value = readValue[Option[T]](reader)
            val optionValue =
                if value.hasValue then new ReadOptionValue[String, T](value.key, value.value)
                else new ReadOptionValue[String, T](value.key, forceRead(reader, _))
            value.updateValue = optionValue;
            return optionValue

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
        def readMembers[T](keys: Array[(String, Decoder[_ <: T])]): ReadValue[String, T] =
            val res = new ReadValue[String, T](forceRead(reader, _))
            for (key, valueDecoder) <- keys do
                readKey(reader, key)
                val value = readValue[T](reader)(using valueDecoder.asInstanceOf[Decoder[T]])
                if value.hasValue then return value
                value.updateValue = res
            return res

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
    /**
     * Stores a key-value pair and a decoder that can be used to decode this value.
     *
     * This is used to store an decoder for later if the key cannot be read yet and keep the same type for the decoder and the value.
     *
     * @param value
     *   The key-value pair
     * @param decoder
     *   The decoder
     */
    protected case class ValueDecoder[T](value: ReadValue[String, T], decoder: Decoder[T])

    /** [TODO: description] */
    protected class DecoderInfo:
        val keys: HashMap[String, ValueDecoder[_]] = new HashMap[String, ValueDecoder[_]]()
        var currentKey: Option[String] = None
        var decodeKey: Option[String] = None
        var previous: DecoderInfo = this

    protected var decoderInfo = new DecoderInfo()

    /** Read the next key for which the value should be decoded, if there isn't currently a value being decoded */
    protected def readDecodeKey(reader: Reader): Unit =
        if decoderInfo.decodeKey.isEmpty && reader.hasString then decoderInfo.decodeKey = Some(reader.readString())
    override def readKey(reader: Reader): String =
        readDecodeKey(reader)
        decoderInfo.currentKey = decoderInfo.decodeKey
        return if decoderInfo.decodeKey.isDefined then decoderInfo.decodeKey.get else ""
    override def readKey(reader: Reader, key: String): String =
        readDecodeKey(reader)
        decoderInfo.currentKey = Some(key)
        return key
    override def readValue[T: Decoder](reader: Reader): ReadValue[String, T] =
        if decoderInfo.currentKey.isEmpty then throw new IllegalStateException("Trying to read a value before reading a key.")

        if decoderInfo.decodeKey == decoderInfo.currentKey then
            val key = decoderInfo.decodeKey.get
            decoderInfo.decodeKey = None
            decoderInfo.currentKey = None
            val res = reader.read[T]()
            if reader.hasString then decoderInfo.decodeKey = Some(reader.readString())
            if decoderInfo.decodeKey.isDefined && decoderInfo.keys.contains(decoderInfo.decodeKey.get) then
                decoderInfo.currentKey = decoderInfo.decodeKey
                val valueDecoder = decoderInfo.keys.get(decoderInfo.currentKey.get).get
                readValue(reader)(using valueDecoder.decoder)
            decoderInfo.currentKey = None
            if decoderInfo.keys.contains(key) then
                val valueDecoder = decoderInfo.keys.remove(key).get.asInstanceOf[ValueDecoder[T]]
                valueDecoder.value.value = res
                return valueDecoder.value
            else return new ReadValue[String, T](key, res)
        else
            val readValue = new ReadValue[String, T](decoderInfo.currentKey.get, forceRead(reader, _))
            decoderInfo.keys.addOne((decoderInfo.currentKey.get, ValueDecoder(readValue, summon[Decoder[T]])))
            decoderInfo.currentKey = None
            return readValue
    override def openEncapsulation(reader: Reader): Unit =
        val info = new DecoderInfo()
        info.previous = decoderInfo
        decoderInfo = info
        reader.readMapStart()
    override def openEncapsulation(reader: Reader, amount: Int): Unit =
        val info = new DecoderInfo()
        info.previous = decoderInfo
        decoderInfo = info
        reader.readMapOpen(amount)
    override def closeEncapsulation[T](reader: Reader): Unit =
        decoderInfo = decoderInfo.previous
        reader.readBreak()
    override def forceReadValue(reader: Reader, key: String): Unit =
        while decoderInfo.decodeKey.isDefined do forceReadValue(reader)
    override def forceReadValue(reader: Reader): Unit =
        val tmpCurrentKey = decoderInfo.currentKey
        readDecodeKey(reader)
        while decoderInfo.decodeKey.isDefined && !decoderInfo.keys.contains(decoderInfo.decodeKey.get) do
            reader.skipElement()
            decoderInfo.decodeKey = None
            readDecodeKey(reader)
        if !decoderInfo.decodeKey.isDefined then return
        val valueDecoder = decoderInfo.keys.get(decoderInfo.decodeKey.get).get
        decoderInfo.currentKey = decoderInfo.decodeKey
        readValue(reader)(using valueDecoder.decoder)
        decoderInfo.currentKey = tmpCurrentKey
    override def decodeOption[T: Decoder](reader: Reader): Option[T] =
        return Some(reader.read[T]())

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
    protected var currentKey: Option[String] = None
    override def readKey(reader: Reader): String = return ""
    override def readKey(reader: Reader, key: String): String =
        currentKey = Some(key)
        return key
    override def readValue[T: Decoder](reader: Reader): ReadValue[String, T] =
        val key = if currentKey.isDefined then currentKey.get else id.toString()
        currentKey = None
        if reader.hasBreak then throw reader.unexpectedDataItem(key)
        id += 1
        val res = reader.read[T]()
        return ReadValue[String, T](key, res)
    override def openEncapsulation(reader: Reader): Unit = reader.readArrayStart()
    override def openEncapsulation(reader: Reader, amount: Int): Unit = reader.readArrayOpen(amount)
    override def closeEncapsulation[T](reader: Reader): Unit = reader.readBreak()
    override def forceReadValue(reader: Reader, key: String): Unit = reader.skipElement()
    override def forceReadValue(reader: Reader): Unit = reader.skipElement()
    override def decodeOption[T: Decoder](reader: Reader): Option[T] =
        var res: Option[T] = None
        reader.readArrayOpen(1)
        if !reader.hasBreak then res = Some(reader.read[T]())
        reader.readBreak()
        return res

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
    def readKeyValue[V, T: Decoder](reader: Reader): ReadValue[V, T] =
        if key.isEmpty then throw new IllegalStateException(s"Trying to read a value before reading a key.")
        val res = reader.read[T]()
        val tmpKey = key
        key = None
        return ReadValue(tmpKey.get.asInstanceOf[V], res)
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
        def readMember[K: Decoder, V: Decoder](): ReadValue[K, V] =
            readKey[K](reader)
            readKeyValue[K, V](reader)
