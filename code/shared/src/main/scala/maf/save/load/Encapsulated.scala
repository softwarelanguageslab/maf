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

class ReadValue[K, V]():
    protected var _key: Option[K] = None
    protected var _value: Option[V] = None
    protected var _updateValue: Option[ReadValue[K, V]] = None

    def this(_key: K, _value: V) =
        this()
        this._key = Some(_key)
        this._value = Some(_value)

    def this(_key: K) =
        this()
        this._key = Some(_key)

    def hasValue: Boolean = _value.isDefined
    def value: V =
        if _value.isEmpty then throw new NoSuchElementException(s"The key '${key}' does not have a value.")
        _value.get
    def value_=(newValue: V): Unit =
        _value = Some(newValue)
        if _updateValue.isDefined then _updateValue.get.value = newValue
    def hasKey: Boolean = _key.isDefined

    /** @throws t */
    def key: K =
        if _key.isEmpty then throw new NoSuchElementException("This element does not have a key.")
        _key.get
    def key_=(newKey: K): Unit =
        _key = Some(newKey)
        if _updateValue.isDefined then _updateValue.get.key = newKey
    def updateValue_=(newUpdateValue: ReadValue[K, V]) = _updateValue = Some(newUpdateValue)
    def updateValue = _updateValue

trait AbstractDecoder:
    protected val values = new HashMap[String, Any]()
    protected var currentKey: Option[String] = None
    val mapBasedDecoder: Boolean
    def readKey(reader: Reader): String
    def readKey(reader: Reader, key: String): String
    def readValue[T: Decoder](reader: Reader): ReadValue[String, T]
    def getValue[T](key: String): T =
        if !values.contains(key) then throw new NoSuchElementException(s"The key '${key}' does not have a value.")
        values.get(key).get.asInstanceOf[T]
    def openEncapsulation(reader: Reader): Unit
    def openEncapsulation(reader: Reader, amount: Int): Unit
    def closeEncapsulation[T](reader: Reader, unbounded: Boolean, res: T): Unit

object AbstractDecoder:
    inline def deriveDecoder[T](decoder: AbstractDecoder): Decoder[T] =
        if decoder.mapBasedDecoder then CompactMapBasedCodecs.deriveDecoder[T] else ArrayBasedCodecs.deriveDecoder[T]
    inline def deriveAllDecoders[T](decoder: AbstractDecoder): Decoder[T] =
        if decoder.mapBasedDecoder then CompactMapBasedCodecs.deriveAllDecoders[T] else ArrayBasedCodecs.deriveAllDecoders[T]

trait EncapsulatedDecoder[T] extends Decoder[T]:
    def decoder: AbstractDecoder
    protected given AbstractDecoder = decoder
    override def read(reader: Reader): T =
        decoder.openEncapsulation(reader)
        val res = readEncapsulated(reader)(using decoder)
        decoder.closeEncapsulation(reader, true, res)
        res
    protected def readEncapsulated(reader: Reader)(using AbstractDecoder): T
trait EncapsulatedArrayDecoder[T](length: Int = 0) extends EncapsulatedDecoder[T]:
    override def read(reader: Reader): T =
        if length == 0 then reader.readArrayStart() else reader.readArrayOpen(length)
        val res = readEncapsulated(reader)(using decoder)
        reader.readArrayClose(true, res)

object EncapsulatedDecoder:
    extension (reader: Reader)
        def readMember[T: Decoder](key: String)(using decoder: AbstractDecoder): ReadValue[String, T] =
            decoder.readKey(reader, key)
            decoder.readValue[T](reader)
        def readMember[T: Decoder]()(using decoder: AbstractDecoder): ReadValue[String, T] =
            decoder.readKey(reader)
            decoder.readValue[T](reader)
        def readMembers[T](keys: Array[(String, Decoder[_ <: T])])(using decoder: AbstractDecoder): ReadValue[String, T] =
            val res = new ReadValue[String, T]()
            for (key, valueDecoder) <- keys do
                decoder.readKey(reader, key)
                val value = decoder.readValue[T](reader)(using valueDecoder.asInstanceOf[Decoder[T]])
                if value.hasValue then return value
                value.updateValue = res
            return res
        def readUntilBeforeBreak[T](zero: T)(f: T => T): T =
            var res = zero
            while !reader.hasBreak do res = f(res)
            return res
        def getMember[T](key: String)(using decoder: AbstractDecoder): T = decoder.getValue[T](key)

class MapDecoder extends AbstractDecoder:
    protected case class ValueDecoder[T](value: ReadValue[String, T], decoder: Decoder[T])
    protected val keys = new HashMap[String, ValueDecoder[_]]()
    protected var decodeKey: Option[String] = None

    override val mapBasedDecoder: Boolean = true
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
                val valueDecoder = keys.remove(decodeKey.get).get
                val res = readValue(reader)(using valueDecoder.decoder)
                valueDecoder.value.value = res.value
            currentKey = None
            return new ReadValue[String, T](key, res)
        else
            val readValue = new ReadValue[String, T](currentKey.get)
            keys.addOne((currentKey.get, ValueDecoder(readValue, summon[Decoder[T]])))
            currentKey = None
            return readValue
    override def openEncapsulation(reader: Reader): Unit = reader.readMapStart()
    override def openEncapsulation(reader: Reader, amount: Int): Unit = reader.readMapOpen(amount)
    override def closeEncapsulation[T](reader: Reader, unbounded: Boolean, res: T): Unit = reader.readMapClose(unbounded, res)

class ArrayDecoder extends AbstractDecoder:
    protected var id = -1

    override val mapBasedDecoder: Boolean = false
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

class ArrayKeyDecoder extends MapDecoder:
    var key: Option[Any] = None
    def readKey[T: Decoder](reader: Reader): Any =
        key = Some(reader.read[T]())
        key.get
    def readKeyValue[V, T: Decoder](reader: Reader): ReadValue[V, T] =
        if key.isEmpty then throw new IllegalStateException(s"Trying to read a value before reading a key.")
        val res = reader.read[T]()
        val tmpKey = key
        key = None
        return ReadValue(tmpKey.get.asInstanceOf[V], res)
    override def openEncapsulation(reader: Reader): Unit = reader.readArrayStart()
    override def openEncapsulation(reader: Reader, amount: Int): Unit = reader.readArrayOpen(amount)
    override def closeEncapsulation[T](reader: Reader, unbounded: Boolean, res: T): Unit = reader.readMapClose(unbounded, res)

object ArrayKeyDecoder:
    extension (reader: Reader)
        def readMember[K: Decoder, V: Decoder]()(using decoder: ArrayKeyDecoder): ReadValue[K, V] =
            decoder.readKey[K](reader)
            decoder.readKeyValue[K, V](reader)
