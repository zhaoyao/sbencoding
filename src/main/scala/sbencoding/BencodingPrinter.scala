package sbencoding

import java.io.{ ByteArrayOutputStream, OutputStream }

trait BencodingPrinter extends (BcValue => Array[Byte]) {

  def apply(x: BcValue): Array[Byte] = {
    val out = new ByteArrayOutputStream()
    print(x, out)
    out.toByteArray
  }

  def print(x: BcValue, out: OutputStream): Unit = {
    x match {
      case BcInt(x) =>
        out.write(("i" + x.longValue() + "e").getBytes("ISO-8859-1"))
      case BcString(x) =>
        out.write(String.valueOf(x.length).getBytes("ISO-8859-1"))
        out.write(':')
        out.write(x)
      case BcList(elts) =>
        out.write('l')
        elts.foreach(v => print(v, out))
        out.write('e')
      case BcDict(fields) =>
        out.write('d')
        fields.toList.sortWith((x, y) => x._1 < y._1)
          .foreach { p =>
            printString(p._1, out)
            print(p._2, out)
          }
        out.write('e')
      case BcNil => // ignore

    }
  }

  def printString(s: String, out: OutputStream): Unit = {
    out.write(String.valueOf(s.length).getBytes("ISO-8859-1"))
    out.write(':')
    out.write(s.getBytes("ISO-8859-1"))
  }

}

object BencodingPrinter extends BencodingPrinter