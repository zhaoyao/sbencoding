package sbencoding

import org.specs2.mutable.Specification

/**
 * User: zhaoyao
 * Date: 3/19/15
 * Time: 11:27
 */
class BasicFormatsSpec extends Specification with DefaultBencodingProtocol {

  "The IntBencodingFormat" should {
    "convert an Int to a BcInt" in {
      42.toBencoding mustEqual BcInt(42)
    }
    "convert a BcInt to an Int" in {
      BcInt(42).convertTo[Int] mustEqual 42
    }

    "convert a Long to a BcInt" in {
      7563661897011259335L.toBencoding mustEqual BcInt(7563661897011259335L)
    }
    "convert a BcInt to a Long" in {
      BcInt(7563661897011259335L).convertTo[Long] mustEqual 7563661897011259335L
    }

    "convert a Short to a BcInt" in {
      42.asInstanceOf[Short].toBencoding mustEqual BcInt(42)
    }
    "convert a JsNumber to a Short" in {
      BcInt(42).convertTo[Short] mustEqual 42
    }
  }

  "The BooleanJsonFormat" should {
    "convert true to a BcInt(1)" in { true.toBencoding mustEqual BcInt(1) }
    "convert false to a BcInt(0)" in { false.toBencoding mustEqual BcInt(0) }
    "convert a BcInt(1) to true" in { BcInt(1).convertTo[Boolean] mustEqual true }
    "convert a BcInt(0) to false" in { BcInt(0).convertTo[Boolean] mustEqual false }
  }

  "The CharBencodingFormat" should {
    "convert a Char to a BcString" in {
      'c'.toBencoding mustEqual BcString(Array('c'.toByte))
    }
    "convert a BcString to a Char" in {
      BcString(Array('c'.toByte)).convertTo[Char] mustEqual 'c'
    }
  }


  "The StringJsonFormat" should {
    "convert a String to a BcString" in {
      "Hello".toBencoding mustEqual BcString("Hello".getBytes("UTF-8"))
    }
    "convert a BcString to a String" in {
      BcString("Hello".getBytes("UTF-8")).convertTo[String] mustEqual "Hello"
    }
  }

  "The SymbolJsonFormat" should {
    "convert a Symbol to a BcString" in {
      'Hello.toBencoding mustEqual BcString("Hello".getBytes("UTF-8"))
    }
    "convert a BcString to a Symbol" in {
      BcString("Hello".getBytes("UTF-8")).convertTo[Symbol] mustEqual 'Hello
    }
  }

}
