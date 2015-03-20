package sbencoding

import java.io.{ ByteArrayInputStream, EOFException, InputStream }

import sun.security.pkcs.ParsingException

import scala.collection.mutable.ArrayBuffer
import scala.io.Codec

object BencodingParser {

  class ParsingException(msg: String) extends RuntimeException(msg)

  def apply(string: String)(implicit codec: Codec): BcValue = apply(string.getBytes(codec.charSet))
  def apply(bytes: Array[Byte]): BcValue = new BencodingParser(new ByteArrayInputStream(bytes)).parse()

}

class BencodingParser(input: InputStream) {

  var c: Int = _

  private def next() = {
    c = input.read()
    c
  }

  def readInteger(): Long = {
    var s = ""
    while (c >= '0' && c <= '9') {
      s += c.toChar
      if (next() == -1) {
        throw new EOFException()
      }
    }
    s.toLong
  }

  def read(n: Int) = {
    val b = new Array[Byte](n)
    b(0) = c.toByte //current byte
    val r = input.read(b, 1, n - 1)
    if (r < n - 1) {
      throw new EOFException()
    }
    b
  }

  def parse() = {
    next()
    val ret = value()
    if (c != -1) {
      throw new ParsingException("Invalid input")
    }
    ret
  }

  def value(): BcValue = {
    c match {
      case -1 =>
        //eof
        throw new EOFException()

      case 'l' =>
        list()

      case 'd' =>
        // dict
        dict()

      case 'i' =>
        int()

      case _ if c >= '0' && c <= '9' =>
        string()

    }
  }

  def int() = {
    assertAndNext('i')
    val i = readInteger()
    assertAndNext('e')
    BcInt(i)
  }

  def assertAndNext(required: Byte): Unit = {
    if (c == required) next()
    else throw new ParsingException("'" + required + "' is required, but got: " + c)
  }

  def list() = {
    assertAndNext('l')

    val values = new ArrayBuffer[BcValue]()
    while (c != 'e') {
      values += value()
    }

    assertAndNext('e')
    BcList(values: _*)
  }

  def string() = {
    val length = readInteger()
    assertAndNext(':')

    val s = BcString(read(length.toInt))
    next()
    s
  }

  def dict(): BcDict = {
    assertAndNext('d')

    val fields = new ArrayBuffer[BcField]()
    while (c != 'e') {
      val k = string()
      val v = value()

      fields += (new String(k.value) -> v)
    }

    assertAndNext('e')
    BcDict(fields: _*)
  }
}