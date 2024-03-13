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
trait AbstractDecoder:
    /** The values that have already been decoded, with their key. */
    protected val values = new HashMap[String, Any]()

    /**
     * The key that should be read next.
     *
     * This is the key that is given through [[readKey]].
     */
    protected var currentKey: Option[String] = None

    /** Does this decoder use maps or array to decode the data. */
    protected val mapBasedDecoder: Boolean

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
     * Returns an already read value using a given key.
     *
     * @param key
     *   The key for which to get the value
     *
     * @throws NoSuchElementException
     *   If this key hasn't been read yet, and therefore doesn't have a value.
     */
    def getValue[T](key: String): T =
        if !values.contains(key) then throw new NoSuchElementException(s"The key '${key}' does not have a value.")
        values.get(key).get.asInstanceOf[T]

    /**
     * Returns whether or not a given key has a value yet.
     *
     * @param key
     *   The key to check if it has a value
     */
    def hasValue(key: String): Boolean = values.contains(key)

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
    def closeEncapsulation[T](reader: Reader, unbounded: Boolean, res: T): Unit

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

object AbstractDecoder:
    /**
     * Automatically derive an decoder for type T.
     *
     * This will derive an decoder for type T, this decoder will either be [[CompactMapBasedCodecs map-based]] or [[ArrayBasedCodecs array-based]]
     * based on the type of the provided decoder.
     *
     * @note
     *   T must be a case class, enum, sealed abstract class or sealed trait
     *
     * @param decoder
     *   The type of decoder that should be used to decode T
     * @tparam T
     *   The type to derive decoders for
     */
    inline def deriveDecoder[T](decoder: AbstractDecoder): Decoder[T] =
        if decoder.mapBasedDecoder then CompactMapBasedCodecs.deriveDecoder[T] else ArrayBasedCodecs.deriveDecoder[T]

    /**
     * Automatically derive all decoders for type T.
     *
     * This will derive an decoder for the type T and all subtypes of type T, these decoders will either be [[CompactMapBasedCodecs map-based]] or
     * [[ArrayBasedCodecs array-based]] based on the type of the provided decoder.
     *
     * @note
     *   T must be a case object, case class, sealed trait or sealed abstract class.
     *
     * @param decoder
     *   The type of decoder that should be used to decode T
     * @tparam T
     *   The type to derive decoders for
     */
    inline def deriveAllDecoders[T](decoder: AbstractDecoder): Decoder[T] =
        if decoder.mapBasedDecoder then CompactMapBasedCodecs.deriveAllDecoders[T] else ArrayBasedCodecs.deriveAllDecoders[T]
    def forceRead(reader: Reader, decoder: AbstractDecoder, key: Option[String]) =
        if key.isDefined then decoder.forceReadValue(reader, key.get)
        else decoder.forceReadValue(reader)

/**
 * Trait used to decode an instance of type T.
 *
 * This trait will read a value of type T encapsulated in either an array or a map, based on the decoder that is given. In order to implement this
 * class, you should overwrite the [[decoder]] variable to decide on the type of decoder you want to use, and the [[readEncapsulated]] method which
 * will implement the actual reading of the value.
 *
 * {{{
 * given EncapsulatedDecoder[< T >] with
 *     override def decoder: AbstractDecoder = < getDecoder >
 *     override protected def readEncapsulated(reader: Reader)(using AbstractDecoder): T = < decode value >
 * }}}
 *
 * @note
 *   Since this class already opens a map/array, you should not do this anymore unless you are reading a nested map/array.
 *
 * @example
 *   If you implement it as a given, you can use it implicitly in your code
 * {{{
 * override protected def readEncapsulated(reader: Reader)(using AbstractDecoder): T =
 *     ...
 *     // Will use the implicit Decoder[T]
 *     reader.readMember[T]("< key >", < value >)
 *     ...
 * }}}
 *
 * @tparam T
 *   The type to decode
 */
trait EncapsulatedDecoder[T] extends Decoder[T]:
    /** The decoder used to read this value, this will specify how this value will be written (e.g. in a map or in an array). */
    def decoder: AbstractDecoder

    /**
     * The decoder used to read this value, this will specify how this value will be written (e.g. in a map or in an array).
     *
     * This is an given because a lot of method require this decoder to be added implicitly, adding the implicit here, makes it that you don't have to
     * specify this anymore.
     */
    protected given AbstractDecoder = decoder
    override def read(reader: Reader): T =
        decoder.openEncapsulation(reader)
        val res = readEncapsulated(reader)(using decoder)
        // Read all remaining elements, since these where not read they will be ignored
        EncapsulatedDecoder.readUntilBeforeBreak(reader)(None,
                                                         (none: None.type) =>
                                                             reader.skipElement()
                                                             None
        )
        decoder.closeEncapsulation(reader, true, res)
        res

    /**
     * Read a value encapsulated in a map/array based on the [[decoder]].
     *
     * @note
     *   This should not be called directly, but only through the [[read]] method.
     *
     * @param reader
     *   The reader used to read the value
     * @param decoder
     *   Implicit argument used to decode the value
     * @returns
     *   The returned value
     */
    protected def readEncapsulated(reader: Reader)(using AbstractDecoder): T

/**
 * Trait used to decode an instance of type T.
 *
 * This trait will read a value of type T encapsulated in an array. In order to implement this class, you should overwrite the [[decoder]] variable to
 * decide on the type of decoder you want to use, and the [[readEncapsulated]] method which will implement the actual reading of the value.
 *
 * {{{
 * given EncapsulatedDecoder[< T >] with
 *     override def decoder: AbstractDecoder = < getDecoder >
 *     override protected def readEncapsulated(reader: Reader)(using AbstractDecoder): T = < decode value >
 * }}}
 *
 * @note
 *   Since this class already opens an array, you should not do this anymore unless you are reading a nested map/array.
 *
 * @note
 *   Using `reader.readMember`, `reader.openEncapsulation`, ... can still use either a map or an array, it is only the top-level encapsulation that is
 *   an array.
 *
 * @example
 *   If you implement it as a given, you can use it implicitly in your code
 * {{{
 * override protected def readEncapsulated(reader: Reader)(using AbstractDecoder): T =
 *     ...
 *     // Will use the implicit Decoder[T]
 *     reader.readMember[T]("< key >", < value >)
 *     ...
 * }}}
 *
 * @tparam T
 *   The type to decode
 */
trait EncapsulatedArrayDecoder[T](length: Int = 0) extends EncapsulatedDecoder[T]:
    override def read(reader: Reader): T =
        if length == 0 then reader.readArrayStart() else reader.readArrayOpen(length)
        val res = readEncapsulated(reader)(using decoder)
        reader.readArrayClose(true, res)

/**
 * Object with extension methods for [[borer.Reader]].
 *
 * These extension methods should be used when using an [[EncapsulatedDecoder]].
 */
object EncapsulatedDecoder:
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
        def readMember[T: Decoder](key: String)(using decoder: AbstractDecoder): ReadValue[String, T] =
            decoder.readKey(reader, key)
            decoder.readValue[T](reader)

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
        def readMember[T: Decoder]()(using decoder: AbstractDecoder): ReadValue[String, T] =
            decoder.readKey(reader)
            decoder.readValue[T](reader)

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
        def readMembers[T](keys: Array[(String, Decoder[_ <: T])])(using decoder: AbstractDecoder): ReadValue[String, T] =
            val res = new ReadValue[String, T](AbstractDecoder.forceRead(reader, decoder, _))
            for (key, valueDecoder) <- keys do
                decoder.readKey(reader, key)
                val value = decoder.readValue[T](reader)(using valueDecoder.asInstanceOf[Decoder[T]])
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

        /**
         * Returns an already read value using a given key.
         *
         * @param decoder
         *   The decoder used to decode the value
         * @throws NoSuchElementException
         *   If this key hasn't been read yet, and therefore doesn't have a value.
         */
        def getMember[T](key: String)(using decoder: AbstractDecoder): T =
            if !decoder.hasValue(key) then decoder.forceReadValue(reader, key)
            decoder.getValue[T](key)

/**
 * Decoder that uses maps to decode values.
 *
 * This decoder uses maps to decode values and therefore requires keys to be present.
 */
class MapDecoder extends AbstractDecoder:
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

    /** Stores keys that haven't been read yet combined with a decoder to decode the value, and the key-value pair that was returned. */
    protected val keys = new HashMap[String, ValueDecoder[_]]()

    /** The key that was read, and which value should now be read. */
    protected var decodeKey: Option[String] = None

    override protected val mapBasedDecoder: Boolean = true

    /** Read the next key for which the value should be decoded, if there isn't currently a value being decoded */
    protected def readDecodeKey(reader: Reader): Unit =
        if decodeKey.isEmpty && reader.hasString then decodeKey = Some(reader.readString())
    override def readKey(reader: Reader): String =
        readDecodeKey(reader)
        this.currentKey = decodeKey
        return if decodeKey.isDefined then decodeKey.get else ""
    override def readKey(reader: Reader, key: String): String =
        readDecodeKey(reader)
        this.currentKey = Some(key)
        return key
    override def readValue[T: Decoder](reader: Reader): ReadValue[String, T] =
        if currentKey.isEmpty then throw new IllegalStateException("Trying to read a value before reading a key.")

        if decodeKey == currentKey then
            val key = decodeKey.get
            decodeKey = None
            currentKey = None
            val res = reader.read[T]()
            values.addOne((key, res))
            if reader.hasString then decodeKey = Some(reader.readString())
            if decodeKey.isDefined && keys.contains(decodeKey.get) then
                currentKey = decodeKey
                val valueDecoder = keys.get(currentKey.get).get
                readValue(reader)(using valueDecoder.decoder)
            currentKey = None
            if keys.contains(key) then
                val valueDecoder = keys.remove(key).get.asInstanceOf[ValueDecoder[T]]
                valueDecoder.value.value = res
                return valueDecoder.value
            else return new ReadValue[String, T](key, res)
        else
            val readValue = new ReadValue[String, T](currentKey.get, AbstractDecoder.forceRead(reader, this, _))
            keys.addOne((currentKey.get, ValueDecoder(readValue, summon[Decoder[T]])))
            currentKey = None
            return readValue
    override def openEncapsulation(reader: Reader): Unit = reader.readMapStart()
    override def openEncapsulation(reader: Reader, amount: Int): Unit = reader.readMapOpen(amount)
    override def closeEncapsulation[T](reader: Reader, unbounded: Boolean, res: T): Unit = reader.readMapClose(unbounded, res)
    override def forceReadValue(reader: Reader, key: String): Unit =
        while decodeKey.isDefined && !values.contains(key.asInstanceOf[String]) do forceReadValue(reader)
    override def forceReadValue(reader: Reader): Unit =
        val tmpCurrentKey = currentKey
        readDecodeKey(reader)
        while decodeKey.isDefined && !keys.contains(decodeKey.get) do
            reader.skipElement()
            decodeKey = None
            readDecodeKey(reader)
        if !decodeKey.isDefined then return
        val valueDecoder = keys.get(decodeKey.get).get
        currentKey = decodeKey
        readValue(reader)(using valueDecoder.decoder)
        currentKey = tmpCurrentKey

/**
 * Decoder that uses arrays to decode values.
 *
 * This decoder uses arrays to decode values and does not require keys, if you want an array-based decoder that saves keys, you should use
 * [[ArrayKeyDecoder]].
 */
class ArrayDecoder extends AbstractDecoder:
    /** Used to generate IDs if no key is provided, this is used to store the values. */
    protected var id = -1

    override protected val mapBasedDecoder: Boolean = false
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
        values.addOne(key, res)
        return ReadValue[String, T](key, res)
    override def openEncapsulation(reader: Reader): Unit = reader.readArrayStart()
    override def openEncapsulation(reader: Reader, amount: Int): Unit = reader.readArrayOpen(amount)
    override def closeEncapsulation[T](reader: Reader, unbounded: Boolean, res: T): Unit = reader.readMapClose(unbounded, res)
    override def forceReadValue(reader: Reader, key: String): Unit = return
    override def forceReadValue(reader: Reader): Unit = reader.skipElement()

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
 */
class ArrayKeyDecoder extends MapDecoder:
    override protected val mapBasedDecoder: Boolean = false

    /** The key that was read, and which value should now be read. */
    protected var key: Option[Any] = None
    protected val keyValues = new HashMap[Any, Any]()

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
        keyValues.addOne((tmpKey.get, res))
        return ReadValue(tmpKey.get.asInstanceOf[V], res)
    override def openEncapsulation(reader: Reader): Unit = reader.readArrayStart()
    override def openEncapsulation(reader: Reader, amount: Int): Unit = reader.readArrayOpen(amount)
    override def closeEncapsulation[T](reader: Reader, unbounded: Boolean, res: T): Unit = reader.readMapClose(unbounded, res)

    /**
     * Returns an already read value using a given key.
     *
     * @throws NoSuchElementException
     *   If this key hasn't been read yet, and therefore doesn't have a value.
     */
    def getValue[T](key: Any): T =
        if !keyValues.contains(key) then
            if key.isInstanceOf[String] then return super.getValue(key.asInstanceOf[String])
            else throw new NoSuchElementException(s"The key '${key}' does not have a value.")
        return keyValues.get(key).get.asInstanceOf[T]

    /**
     * Returns whether or not a given key has a value yet.
     *
     * @param key
     *   The key to check if it has a value
     */
    def hasValue(key: Any): Boolean = keyValues.contains(key) || (key.isInstanceOf[String] && super.hasValue(key.asInstanceOf[String]))

/**
 * Object with an extension method for [[borer.Reader]].
 *
 * This extension method allows you to read a key-value pair where the key is not a String, and should only be used when using an [[ArrayKeyDecoder]].
 */
object ArrayKeyDecoder:
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
        def readMember[K: Decoder, V: Decoder]()(using decoder: ArrayKeyDecoder): ReadValue[K, V] =
            decoder.readKey[K](reader)
            decoder.readKeyValue[K, V](reader)
