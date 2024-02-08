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

trait AbstractDecoder:
    val mapBasedDecoder: Boolean
    def readKey(reader: Reader): Unit
    def readKey(reader: Reader, key: String): Unit
    def readValue[T: Decoder](reader: Reader): Future[T]
    def getValue[T](key: String): T
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

object EncapsulatedDecoder:
    extension (reader: Reader)
        def readMember[T: Decoder](key: String)(using decoder: AbstractDecoder): Future[T] =
            decoder.readKey(reader, key)
            decoder.readValue[T](reader)
        def readMembers[T](keys: Array[(String, Decoder[_ <: T])])(using decoder: AbstractDecoder): Future[T] =
            val promise = Promise[T]()
            for (key, valueDecoder) <- keys do
                decoder.readKey(reader, key)
                val value = decoder.readValue(reader)(using valueDecoder)
                if value.isCompleted then
                    promise.complete(value.value.get)
                    return promise.future
                value.onComplete {
                    case Success(value) => promise.success(value.asInstanceOf[T])
                    case Failure(e)     => promise.failure(e.asInstanceOf[Throwable])
                }
            return promise.future

        def getMember[T](key: String)(using decoder: AbstractDecoder): T = decoder.getValue[T](key)

class MapDecoder extends AbstractDecoder:
    protected val values = new HashMap[String, Any]()
    protected val keys = new HashMap[String, Decoder[_]]()
    protected var decodeKey: Option[String] = None
    protected var currentKey: Option[String] = None

    override val mapBasedDecoder: Boolean = true
    protected def readDecodeKey(reader: Reader): Unit =
        if decodeKey.isEmpty && reader.hasString then decodeKey = Some(reader.readString())
    override def readKey(reader: Reader): Unit =
        readDecodeKey(reader)
        this.currentKey = decodeKey
    override def readKey(reader: Reader, key: String): Unit =
        readDecodeKey(reader)
        this.currentKey = Some(key)
    override def readValue[T: Decoder](reader: Reader): Future[T] =
        if currentKey.isEmpty then throw new KeyException("Trying to read a value before reading a key.")
        val promise = Promise[T]()

        if decodeKey == currentKey then
            val key = decodeKey.get
            decodeKey = None
            currentKey = None
            val res = reader.read[T]()
            values.addOne((key, res))
            if reader.hasString then decodeKey = Some(reader.readString())
            promise.success(res)
            if decodeKey.isDefined && keys.contains(decodeKey.get) then
                currentKey = decodeKey
                val decoder = keys.remove(decodeKey.get).get
                readValue(reader)(using decoder)
        else keys.addOne((currentKey.get, summon[Decoder[T]]))
        currentKey = None
        promise.future
    override def getValue[T](key: String): T =
        if !values.contains(key) then throw new KeyException("Key '" + key + "' is not found.")
        values.get(key).get.asInstanceOf[T]
    override def openEncapsulation(reader: Reader): Unit = reader.readMapStart()
    override def openEncapsulation(reader: Reader, amount: Int): Unit = reader.readMapOpen(amount)
    override def closeEncapsulation[T](reader: Reader, unbounded: Boolean, res: T): Unit = reader.readMapClose(unbounded, res)

    case class KeyException(msg: String) extends Exception(msg)
