package sbencoding

import scala.io.Codec

/**
 * Provides the BencodingFormats for the most important Scala types.
 */
trait BasicFormats {

  implicit object IntBencodingFormat extends BencodingFormat[Int] {
    def write(x: Int) = BcInt(x)
    def read(value: BcValue) = value match {
      case BcInt(x) => x.intValue
      case x        => deserializationError("Expected Int as BcNumber, but got " + x.getClass.getSimpleName)
    }
  }

  implicit object LongBencodingFormat extends BencodingFormat[Long] {
    def write(x: Long) = BcInt(x)
    def read(value: BcValue) = value match {
      case BcInt(x) => x.longValue
      case x        => deserializationError("Expected Long as BcNumber, but got " + x.getClass.getSimpleName
    }
  }

  implicit object ByteBencodingFormat extends BencodingFormat[Byte] {
    def write(x: Byte) = BcInt(x)
    def read(value: BcValue) = value match {
      case BcInt(x) => x.byteValue
      case x        => deserializationError("Expected Byte as BcNumber, but got " + x.getClass.getSimpleName)
    }
  }

  implicit object ShortBencodingFormat extends BencodingFormat[Short] {
    def write(x: Short) = BcInt(x)
    def read(value: BcValue) = value match {
      case BcInt(x) => x.shortValue
      case x        => deserializationError("Expected Short as BcNumber, but got " + x.getClass.getSimpleName)
    }
  }

  implicit object BooleanBencodingFormat extends BencodingFormat[Boolean] {
    def write(x: Boolean) = BcInt(if (x) 1 else 0)
    def read(value: BcValue) = value match {
      case BcInt(1) => true
      case BcInt(0) => false
      case x        => deserializationError("Expected JsBoolean, but got " + x.getClass.getSimpleName)
    }
  }

  implicit object CharBencodingFormat extends BencodingFormat[Char] {
    def write(x: Char) = {
      if (x < 0xff)
        BcString(x.toString, "ascii")
      else
        serializationError("Expected Char in range 0x00 to 0xff, but got " + x.getClass.getSimpleName)
    }
    def read(value: BcValue) = value match {
      case BcString(x) if x.length == 1 => x(0).toChar
      case x                            => deserializationError("Expected Char as single-character JsString, but got " + x.getClass.getSimpleName)
    }
  }

  /**
   * Bencoding does not define how to handle non-ascii characters.
   * It depends on the original charset. We use utf-8 to en/decode string value.
   * If it's not the correct charset, user should recreate the string object:
   *  new String(s.getBytes("UTF-8"), originalCharset)
   */
  implicit object StringBencodingFormat extends BencodingFormat[String] {
    def write(x: String) = {
      require(x ne null)
      BcString(x.getBytes("UTF-8"))
    }
    def read(value: BcValue) = value match {
      case BcString(x) => new String(x, "UTF-8")
      case x           => deserializationError("Expected String as BcString, but got " + x.getClass.getSimpleName)
    }
  }

  implicit object SymbolBencodingFormat extends BencodingFormat[Symbol] {
    def write(x: Symbol) = StringBencodingFormat.write(x.name)
    def read(value: BcValue) = Symbol(StringBencodingFormat.read(value))
  }
}