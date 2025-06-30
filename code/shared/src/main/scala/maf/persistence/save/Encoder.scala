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

/**
 * Base trait for an encoder.
 *
 * This trait has methods for writing keys and values with a encoder to a given writer and depending on the implementation off this trait, this can
 * either write your values to a map or an array. This allows you to encode your values in a certain way, but without knowing exactly how it will be
 * stored (e.g. in a map or in an array).
 *
 * This trait also has method for opening and closing an encapsulation, this will open/close either a map or an array based on the implementation of
 * this trait.
 *
 * @note
 *   This trait gives the methods needed to write values, but not the implementation. Other traits like [[MapEncoder]] or [[ArrayEncoder]] should be
 *   used depending on how you want your value to be encoded.
 *
 * @note
 *   Some encoders like [[ArrayEncoder]] will also not encode your key, because arrays do not require keys like maps do, if the key does need to be
 *   stored, you should use an encoder that writes them down like [[MapEncoder]] or [[ArrayKeyEncoder]] if you want to use an array.
 */
trait AbstractEncoder[T] extends Encoder[T]:
    /**
     * Write a generated key to the given writer.
     *
     * @note
     *   This only writes a key and should therefore be followed up by either writing a value using [[writeValue]] or opening an map or array using
     *   [[openEncapsulation]].
     *
     * @note
     *   In some encoders like [[ArrayEncoder]] this will not write anything as this encoder does not use keys.
     *
     * @note
     *   The key that is generated depends on the encoder that is used.
     *
     * @param writer
     *   The writer used write the key
     * @return
     *   The used writer
     */
    def writeKey(writer: Writer): Writer

    /**
     * Write the given key to the given writer.
     *
     * @note
     *   This only writes a key and should therefore be followed up by either writing a value using [[writeValue]] or opening an map or array using
     *   [[openEncapsulation]].
     *
     * @note
     *   In some encoders like [[ArrayEncoder]] this will not write anything as this encoder does not use keys.
     *
     * @param writer
     *   The writer used write the key
     * @param key
     *   The key to write
     * @return
     *   The used writer
     */
    def writeKey(writer: Writer, key: String): Writer

    /**
     * Write a given value to the given writer.
     *
     * @note
     *   This only writes a value and should therefore only be called after writing a key using [[writeKey]].
     *
     * @param writer
     *   The writer used write the value
     * @param value
     *   The value to write
     * @return
     *   The used writer
     */
    def writeValue[T: Encoder](writer: Writer, value: T): Writer

    /**
     * Opens either a new map or a new array, based on the encoder that is used.
     *
     * @note
     *   This only opens a new encapsulation and should therefore only be called after writing a key using [[writeKey]].
     *
     * @param writer
     *   The writer used write the array/map
     * @return
     *   The used writer
     */
    def openEncapsulation(writer: Writer): Writer

    /**
     * Opens either a new map or a new array, based on the encoder that is used.
     *
     * @note
     *   This only opens a new encapsulation and should therefore only be called after writing a key using [[writeKey]].
     *
     * @param writer
     *   The writer used write the array/map
     * @param amount
     *   The amount of elements the map/array, this is only useful when using CBOR the length of maps and arrays is used in the encoding
     * @return
     *   The used writer
     */
    def openEncapsulation(writer: Writer, amount: Int): Writer

    /**
     * Closes the map/array, based on the encoder that is used.
     *
     * @note
     *   This only closes the encapsulation and should therefore only be called after having already opened an encapsulation using
     *   [[openEncapsulation]].
     *
     * @param writer
     *   The writer used write the array/map
     * @return
     *   The used writer
     */
    def closeEncapsulation(writer: Writer): Writer = writer.writeBreak()

    /** [TODO: description] */
    def encodeOption[T: Encoder](writer: Writer, key: String, option: Option[T]): Writer

    extension (writer: Writer)
        /**
         * Close the encapsulation.
         *
         * This will close either a map or an array based on the encoder that is given.
         *
         * @param encoder
         *   Implicit argument that decides how to close the encapsulation
         */
        def close(): Writer = closeEncapsulation(writer)

        /**
         * Write a key-value pair.
         *
         * This will write a key-value pair in either a map or an array, based on the encoder that is given.
         *
         * @param key
         *   The key to write
         * @param value
         *   The value to write
         * @param encoder
         *   Implicit argument that decides how to write the key-value pair
         * @tparam T
         *   The type of the value that should be written, this type should have an encoder
         */
        def writeMember[T: Encoder](key: String, value: T): Writer =
            writeKey(writer, key)
            writeValue(writer, value)

        /**
         * Write a value.
         *
         * This will write a value in either a map or an array, based on the encoder that is given. Certain encoders like map-based encoders will
         * create a key and write this first if this method is called.
         *
         * @param value
         *   The value to write
         * @param encoder
         *   Implicit argument that decides how to write the value
         * @tparam T
         *   The type of the value that should be written, this type should have an encoder
         */
        def writeMember[T: Encoder](value: T): Writer =
            writeKey(writer)
            writeValue(writer, value)

        def writeMember[T: Encoder](key: String, value: Option[T]): Writer =
            encodeOption(writer, key, value)

        /** [TODO: description] */
        def start() =
            openEncapsulation(writer)

        /**
         * Open the encapsulation.
         *
         * This will open either a map or an array based on the encoder that is given. Certain encoders like map-based encoders will create a key and
         * write this first if this method is called.
         *
         * @param encoder
         *   Implicit argument that decides how to open the encapsulation
         */
        def open(): Writer =
            writeKey(writer)
            openEncapsulation(writer)

        /**
         * Open the encapsulation with a key.
         *
         * This will open either a map or an array based on the encoder that is given with a given key.
         *
         * @param key
         *   The key to write
         * @param encoder
         *   Implicit argument that decides how to open the encapsulation
         */
        def open(key: String): Writer =
            writeKey(writer, key)
            openEncapsulation(writer)

        /**
         * Open the encapsulation.
         *
         * This will open either a map or an array based on the encoder that is given. Certain encoders like map-based encoders will create a key and
         * write this first if this method is called.
         *
         * @param amount
         *   The amount of elements the map/array, this is only useful when using CBOR the length of maps and arrays is used in the encoding
         * @param encoder
         *   Implicit argument that decides how to open the encapsulation
         */
        def open(amount: Int): Writer =
            writeKey(writer)
            openEncapsulation(writer, amount)

        /**
         * Open the encapsulation with a key.
         *
         * This will open either a map or an array based on the encoder that is given with a given key.
         *
         * @param key
         *   The key to write
         * @param amount
         *   The amount of elements the map/array, this is only useful when using CBOR the length of maps and arrays is used in the encoding
         * @param encoder
         *   Implicit argument that decides how to open the encapsulation
         */
        def open(key: String, amount: Int): Writer =
            writeKey(writer, key)
            openEncapsulation(writer)

/**
 * Encoder that uses maps to encode values.
 *
 * This encoder uses maps to encode values and will therefore always use keys, if no key is provided, an auto-increasing ID will be used instead.
 *
 * @example
 * {{{
 * given MapEncoder[T]
 *     override protected def write(writer: Writer, value: T): Writer =
 *           writer.start()
 *           < encode value >
 *           writer.close()
 * }}}
 *
 * @tparam T
 *   The type to encode
 */
trait MapEncoder[T] extends AbstractEncoder[T]:
    /** Used to generate IDs if no key is provided */
    private var id = -1
    override def writeKey(writer: Writer): Writer =
        id += 1
        writeKey(writer, id.toString())
    override def writeKey(writer: Writer, key: String): Writer = writer.write(key)
    override def writeValue[T: Encoder](writer: Writer, value: T): Writer = writer.write(value)
    override def openEncapsulation(writer: Writer): Writer = writer.writeMapStart()
    override def openEncapsulation(writer: Writer, amount: Int): Writer = writer.writeMapOpen(amount)
    override def encodeOption[T: Encoder](writer: Writer, key: String, option: Option[T]): Writer =
        if option.isDefined then writer.writeMember(key, option.get)
        writer

/**
 * Encoder that uses arrays to encode values.
 *
 * This encoder uses arrays to encode values and will not write any keys, if you want an array-based encoder that saves keys, you should use
 * [[ArrayKeyEncoder]].
 *
 * @example
 * {{{
 * given ArrayEncoder[T]
 *     override protected def write(writer: Writer, value: T): Writer =
 *           writer.start()
 *           < encode value >
 *           writer.close()
 * }}}
 *
 * @tparam T
 *   The type to encode
 */
trait ArrayEncoder[T] extends AbstractEncoder[T]:
    override def writeKey(writer: Writer): Writer = writer
    override def writeKey(writer: Writer, key: String): Writer = writer
    override def writeValue[T: Encoder](writer: Writer, value: T): Writer = writer.write(value)
    override def openEncapsulation(writer: Writer): Writer = writer.writeArrayStart()
    override def openEncapsulation(writer: Writer, amount: Int): Writer = writer.writeArrayOpen(amount)
    override def encodeOption[T: Encoder](writer: Writer, key: String, option: Option[T]): Writer =
        writer.writeArrayStart()
        if option.isDefined then writer.write(option.get)
        writer.writeBreak()

/**
 * Encoder that uses arrays to encode values, but preserves keys.
 *
 * This encoder uses arrays to encode values but will save key-value pairs by first writing the key and then the value. This can be used, for example,
 * if your keys are not strings. Since non-string keys are not supported in JSON, you cannot use a map for this, but this class does allow you to save
 * your key-value pair in an intuitive way.
 *
 * This is how your key-value pair would be saved:
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
 * given ArrayKeyEncoder[T]
 *     override protected def write(writer: Writer, value: T): Writer =
 *           writer.start()
 *           < encode value >
 *           writer.close()
 * }}}
 *
 * @tparam T
 *   The type to encode
 */
trait ArrayKeyEncoder[T] extends ArrayEncoder[T]:
    override def writeKey(writer: Writer, key: String): Writer = writer.write(key)

    /**
     * Write the given key to the given writer.
     *
     * @note
     *   This only writes a key and should therefore be followed up by either writing a value using [[writeValue]] or opening an map or array using
     *   [[openEncapsulation]].
     *
     * @note
     *   In some encoders like [[ArrayEncoder]] this will not write anything as this encoder does not use keys.
     *
     * @param writer
     *   The writer used write the key
     * @param key
     *   The key to write
     * @return
     *   The used writer
     */
    def writeKey[T: Encoder](writer: Writer, key: T): Writer = writer.write(key)
    override def writeValue[T: Encoder](writer: Writer, value: T): Writer = writer.write(value)
    override def encodeOption[T: Encoder](writer: Writer, key: String, option: Option[T]): Writer =
        if option.isDefined then writer.writeMember(key, option.get)
        writer

    extension (writer: Writer)
        /**
         * Write a key-value pair.
         *
         * This will write a key-value pair in either a map or an array, based on the encoder that is given.
         *
         * @param key
         *   The key to write
         * @param value
         *   The value to write
         * @param encoder
         *   Implicit argument that decides how to write the key-value pair
         * @tparam T
         *   The type of the key that should be written, this type should have an encoder
         * @tparam U
         *   The type of the value that should be written, this type should have an encoder
         */
        def writeMember[T: Encoder, U: Encoder](key: T, value: U): Writer =
            writeKey(writer, key)
            writeValue(writer, value)
