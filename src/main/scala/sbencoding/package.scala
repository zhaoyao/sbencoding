import java.nio.ByteBuffer

/**
 * User: zhaoyao
 * Date: 3/17/15
 * Time: 21:36
 */
package object sbencoding {
  type BcField = (String, BcValue)

  def deserializationError(msg: String, cause: Throwable = null) = throw new DeserializationException(msg, cause)
  def serializationError(msg: String) = throw new SerializationException(msg)

  def bencodingReader[T](implicit reader: BencodingReader[T]) = reader
  def bencodingWriter[T](implicit writer: BencodingWriter[T]) = writer

  implicit def pimpAny[T](any: T) = new PimpedAny(any)
  implicit def pimpString(string: String) = new PimpedString(string)
  implicit def pimpBytes(bytes: Array[Byte]) = new PimpedByteArray(bytes)
  implicit def pimpBytes(bytes: ByteBuffer) = new PimpedByteBuffer(bytes)
}

package sbencoding {

  import java.nio.ByteBuffer

  import scala.io.Codec

  class DeserializationException(msg: String, cause: Throwable = null) extends RuntimeException(msg, cause)
  class SerializationException(msg: String) extends RuntimeException(msg)

  private[sbencoding] class PimpedAny[T](any: T) {
    def toBencoding(implicit writer: BencodingWriter[T]): BcValue = writer.write(any)
  }

  private[sbencoding] class PimpedString(string: String)(implicit codec: Codec) {
    def parseBencoding: BcValue = BencodingParser(string.getBytes(codec.charSet))
  }

  private[sbencoding] class PimpedByteArray(bytes: Array[Byte])(implicit codec: Codec) {
    def parseBencoding: BcValue = BencodingParser(bytes)
  }

  private[sbencoding] class PimpedByteBuffer(buffer: ByteBuffer) {
    def parseBencoding: BcValue = {
      val bytes = new Array[Byte](buffer.remaining())
      buffer.get(bytes)
      BencodingParser(bytes)
    }
  }

}