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
trait AbstractEncoder:
    /** Does this encoder use maps or array to encode the data. */
    protected val mapBasedEncoder: Boolean

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
    def closeEncapsulation(writer: Writer): Writer

object AbstractEncoder:
    /**
     * Automatically derive an encoder for type T.
     *
     * This will derive an encoder for type T, this encoder will either be [[CompactMapBasedCodecs map-based]] or [[ArrayBasedCodecs array-based]]
     * based on the type of the provided encoder.
     *
     * @note
     *   T must be a case class, enum, sealed abstract class or sealed trait
     *
     * @param encoder
     *   The type of encoder that should be used to encode T
     * @tparam T
     *   The type to derive encoders for
     */
    inline def deriveEncoder[T](encoder: AbstractEncoder): Encoder[T] =
        if encoder.mapBasedEncoder then CompactMapBasedCodecs.deriveEncoder[T] else ArrayBasedCodecs.deriveEncoder[T]

    /**
     * Automatically derive all encoders for type T.
     *
     * This will derive an encoder for the type T and all subtypes of type T, these encoders will either be [[CompactMapBasedCodecs map-based]] or
     * [[ArrayBasedCodecs array-based]] based on the type of the provided encoder.
     *
     * @note
     *   T must be a case object, case class, sealed trait or sealed abstract class.
     *
     * @param encoder
     *   The type of encoder that should be used to encode T
     * @tparam T
     *   The type to derive encoders for
     */
    inline def deriveAllEncoders[T](encoder: AbstractEncoder): Encoder[T] =
        if encoder.mapBasedEncoder then CompactMapBasedCodecs.deriveAllEncoders[T] else ArrayBasedCodecs.deriveAllEncoders[T]

/**
 * Trait used to encode an instance of type T.
 *
 * This trait will write a value of type T but encapsulate it either in an array or a map, based on the encoder that is given. In order to implement
 * this class, you should overwrite the [[encoder]] variable to decide on the type of encoder you want to use, and the [[writeEncapsulated]] method
 * which will implement the actual writing of the value.
 *
 * {{{
 * given EncapsulatedEncoder[< T >] with
 *     override val encoder: AbstractEncoder = < getEncoder >
 *     override protected def writeEncapsulated(writer: Writer, value: < T >): Writer = < encode value >
 * }}}
 *
 * @note
 *   Since this class already opens a map/array, you should not do this anymore unless you are writing a nested map/array.
 *
 * @example
 *   If you implement it as a given, you can use it implicitly in your code
 * {{{
 * override protected def writeEncapsulated(writer: Writer, value: < ... >): Writer =
 *     ...
 *     // Will use the implicit Encoder[T]
 *     writer.writeMember[T]("< key >", < value >)
 *     ...
 * }}}
 *
 * @tparam T
 *   The type to encode
 */
trait EncapsulatedEncoder[T] extends Encoder[T]:
    /** The encoder used to write this value, this will specify how this value will be written (e.g. in a map or in an array). */
    val encoder: AbstractEncoder

    /**
     * The encoder used to write this value, this will specify how this value will be written (e.g. in a map or in an array).
     *
     * This is an given because a lot of method require this encoder to be added implicitly, adding the implicit here, makes it that you don't have to
     * specify this anymore.
     */
    protected given AbstractEncoder = encoder
    override def write(writer: Writer, value: T): Writer =
        encoder.openEncapsulation(writer)
        writeEncapsulated(writer, value)
        encoder.closeEncapsulation(writer)

    /**
     * Write a given value encapsulated in a map/array based on the [[encoder]].
     *
     * @note
     *   This should not be called directly, but only through the [[write]] method.
     *
     * @param writer
     *   The writer used to write the value
     * @param value
     *   The value that should be written
     */
    protected def writeEncapsulated(writer: Writer, value: T): Writer

/**
 * Trait used to encode an instance of type T.
 *
 * This trait will write a value of type T but encapsulate it either in an array. In order to implement this class, you should overwrite the
 * [[encoder]] variable to decide on the type of encoder you want to use, and the [[writeEncapsulated]] method which will implement the actual writing
 * of the value.
 *
 * {{{
 * given EncapsulatedEncoder[< T >] with
 *     override val encoder: AbstractEncoder = < getEncoder >
 *     override protected def writeEncapsulated(writer: Writer, value: < T >): Writer = < encode value >
 * }}}
 *
 * @note
 *   Since this class already opens an array, you should not do this anymore unless you are writing a nested map/array.
 *
 * @note
 *   Using `writer.writeMember`, `writer.openEncapsulation`, ... can still use either a map or an array, it is only the top-level encapsulation that
 *   is an array.
 *
 * @example
 *   If you implement it as a given, you can use it implicitly in your code
 * {{{
 * override protected def writeEncapsulated(writer: Writer, value: < ... >): Writer =
 *     ...
 *     // Will use the implicit Encoder[T]
 *     writer.writeMember[T]("< key >", < value >)
 *     ...
 * }}}
 *
 * @tparam T
 *   The type to encode
 */
trait EncapsulatedArrayEncoder[T](length: Int = 0) extends EncapsulatedEncoder[T]:
    override def write(writer: Writer, value: T): Writer =
        if length == 0 then writer.writeArrayStart() else writer.writeArrayOpen(length)
        writeEncapsulated(writer, value)
        writer.writeArrayClose()

/**
 * Object with extension methods for [[borer.Writer]].
 *
 * These extension methods should be used when using an [[EncapsulatedEncoder]].
 */
object EncapsulatedEncoder:
    extension (writer: Writer)
        /**
         * Close the encapsulation.
         *
         * This will close either a map or an array based on the encoder that is given.
         *
         * @param encoder
         *   Implicit argument that decides how to close the encapsulation
         */
        def close()(using encoder: AbstractEncoder): Writer = encoder.closeEncapsulation(writer)

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
        def writeMember[T: Encoder](key: String, value: T)(using encoder: AbstractEncoder): Writer =
            encoder.writeKey(writer, key)
            encoder.writeValue(writer, value)

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
        def writeMember[T: Encoder](value: T)(using encoder: AbstractEncoder): Writer =
            encoder.writeKey(writer)
            encoder.writeValue(writer, value)

        /**
         * Open the encapsulation.
         *
         * This will open either a map or an array based on the encoder that is given. Certain encoders like map-based encoders will create a key and
         * write this first if this method is called.
         *
         * @param encoder
         *   Implicit argument that decides how to open the encapsulation
         */
        def open()(using encoder: AbstractEncoder): Writer =
            encoder.writeKey(writer)
            encoder.openEncapsulation(writer)

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
        def open(key: String)(using encoder: AbstractEncoder): Writer =
            encoder.writeKey(writer, key)
            encoder.openEncapsulation(writer)

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
        def open(amount: Int)(using encoder: AbstractEncoder): Writer =
            encoder.writeKey(writer)
            encoder.openEncapsulation(writer, amount)

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
        def open(key: String, amount: Int)(using encoder: AbstractEncoder): Writer =
            encoder.writeKey(writer, key)
            encoder.openEncapsulation(writer)

/**
 * Encoder that uses maps to encode values.
 *
 * This encoder uses maps to encode values and will therefore always use keys, if no key is provided, an auto-increasing ID will be used instead.
 */
class MapEncoder extends AbstractEncoder:
    /** Used to generate IDs if no key is provided */
    private var id = -1
    val mapBasedEncoder = true
    override def writeKey(writer: Writer): Writer =
        id += 1
        writeKey(writer, id.toString())
    override def writeKey(writer: Writer, key: String): Writer = writer.write(key)
    override def writeValue[T: Encoder](writer: Writer, value: T): Writer = writer.write(value)
    override def openEncapsulation(writer: Writer): Writer = writer.writeMapStart()
    override def openEncapsulation(writer: Writer, amount: Int): Writer = writer.writeMapOpen(amount)
    override def closeEncapsulation(writer: Writer): Writer = writer.writeMapClose()

/**
 * Encoder that uses arrays to encode values.
 *
 * This encoder uses arrays to encode values and will not write any keys, if you want an array-based encoder that saves keys, you should use
 * [[ArrayKeyEncoder]].
 */
class ArrayEncoder extends AbstractEncoder:
    override val mapBasedEncoder = false
    override def writeKey(writer: Writer): Writer = writer
    override def writeKey(writer: Writer, key: String): Writer = writer
    override def writeValue[T: Encoder](writer: Writer, value: T): Writer = writer.write(value)
    override def openEncapsulation(writer: Writer): Writer = writer.writeArrayStart()
    override def openEncapsulation(writer: Writer, amount: Int): Writer = writer.writeArrayOpen(amount)
    override def closeEncapsulation(writer: Writer): Writer = writer.writeArrayClose()

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
 */
class ArrayKeyEncoder extends ArrayEncoder:
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

/**
 * Object with an extension method for [[borer.Writer]].
 *
 * This extension method allows you to write a key-value pair where the key is not a String, and should only be used when using an
 * [[ArrayKeyEncoder]].
 */
object ArrayKeyEncoder:
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
        def writeMember[T: Encoder, U: Encoder](key: T, value: U)(using encoder: ArrayKeyEncoder): Writer =
            encoder.writeKey(writer, key)
            encoder.writeValue(writer, value)
