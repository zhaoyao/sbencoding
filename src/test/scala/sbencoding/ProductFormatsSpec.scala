package sbencoding

import org.specs2.mutable.Specification

/**
 * User: zhaoyao
 * Date: 3/19/15
 * Time: 13:54
 */
class ProductFormatsSpec extends Specification {
  case class Test0()
  case class Test2(a: Int, b: Option[Int])
  case class Test3[A, B](as: List[A], bs: List[B])
  case class TestTransient(a: Int, b: Option[Int]) {
    @transient var c = false
  }

  @SerialVersionUID(1L) // SerialVersionUID adds a static field to the case class
  case class TestStatic(a: Int, b: Option[Int])
  case class TestMangled(`foo-bar!`: Int)

  trait TestProtocol {
    this: DefaultBencodingProtocol =>
    implicit val test0Format = bencodingFormat0(Test0)
    implicit val test2Format = bencodingFormat2(Test2)
    implicit def test3Format[A: BF, B: BF] = bencodingFormat2(Test3.apply[A, B])
    implicit def testTransientFormat = bencodingFormat2(TestTransient)
    implicit def testStaticFormat = bencodingFormat2(TestStatic)
    implicit def testMangledFormat = bencodingFormat1(TestMangled)
  }
  object TestProtocol1 extends DefaultBencodingProtocol with TestProtocol

  "A BencodingFormat created with `bencodingFormat`, for a case class with 2 elements," should {
    import TestProtocol1._
    val obj = Test2(42, Some(42))
    val bencoding = BcDict("a" -> BcInt(42), "b" -> BcInt(42))

    "convert to a respective BcDict" in {
      obj.toBencoding mustEqual bencoding
    }

    "convert a JsObject to the respective case class instance" in {
      bencoding.convertTo[Test2] mustEqual obj
    }

    "throw a DeserializationException if the JsObject does not all required members" in (
      BcDict("b" -> BcInt(42)).convertTo[Test2] must
      throwA(new DeserializationException("Object is missing required member 'a'"))
    )
    "not require the presence of optional fields for deserialization" in {
      BcDict("a" -> BcInt(42)).convertTo[Test2] mustEqual Test2(42, None)
    }
    "not render `None` members during serialization" in {
      Test2(42, None).toBencoding mustEqual BcDict("a" -> BcInt(42))
    }
    "ignore additional members during deserialization" in {
      BcDict("a" -> BcInt(42), "b" -> BcInt(42), "c" -> BcString('no)).convertTo[Test2] mustEqual obj
    }
    "not depend on any specific member order for deserialization" in {
      BcDict("b" -> BcInt(42), "a" -> BcInt(42)).convertTo[Test2] mustEqual obj
    }

  }

  "A BencodingFormat for a generic case class and created with `bencodingFormat`" should {
    import TestProtocol1._
    val obj = Test3(42 :: 43 :: Nil, "x" :: "y" :: "z" :: Nil)
    val b = BcDict(
      "as" -> BcList(BcInt(42), BcInt(43)),
      "bs" -> BcList(BcString("x", "UTF-8"), BcString("y", "UTF-8"), BcString("z", "UTF-8"))
    )
    "convert to a respective BcDict" in {
      obj.toBencoding mustEqual b
    }
    "convert a BcDict to the respective case class instance" in {
      b.convertTo[Test3[Int, String]] mustEqual obj
    }
  }

  "A BencodingFormat for a case class with 18 parameters and created with `bencodingFormat`" should {

    case class Test18(
      a1: String,
      a2: String,
      a3: String,
      a4: String,
      a5: Int,
      a6: String,
      a7: String,
      a8: String,
      a9: String,
      a10: String,
      a11: String,
      a12: Int,
      a13: String,
      a14: String,
      a15: String,
      a16: String,
      a17: String,
      a18: String)

    object Test18Protocol extends DefaultBencodingProtocol {
      implicit val test18Format = bencodingFormat18(Test18)
    }

    import Test18Protocol._
    val obj = Test18("a1", "a2", "a3", "a4", 5, "a6", "a7", "a8", "a9",
      "a10", "a11", 12, "a13", "a14", "a15", "a16", "a17", "a18")

    val b = BencodingParser("d2:a12:a12:a22:a22:a32:a32:a42:a42:a5i5e2:a62:a62:a72:a72:a82:a82:a92:a93:a103:a103:a113:a113:a12i12e3:a133:a133:a143:a143:a153:a153:a163:a163:a173:a173:a183:a18e")
    "convert to a respective BcDict" in {
      obj.toBencoding mustEqual b
    }
    "convert a BcDict to the respective case class instance" in {
      b.convertTo[Test18] mustEqual obj
    }
  }

  "A BencodingFormat for a generic case class with an explicitly provided type parameter" should {
    "support the bencodingFormat1 syntax" in {
      case class Box[A](a: A)
      object BoxProtocol extends DefaultBencodingProtocol {
        implicit val boxFormat = bencodingFormat1(Box[Int])
      }
      import BoxProtocol._
      Box(42).toBencoding === BcDict("a" -> BcInt(42))
    }
  }

  "A BencodingFormat for a case class with transient fields and created with `bencodingFormat`" should {
    import TestProtocol1._
    val obj = TestTransient(42, Some(42))
    val b = BcDict("a" -> BcInt(42), "b" -> BcInt(42))
    "convert to a respective BcDict" in {
      obj.toBencoding mustEqual b
    }
    "convert a BcDict to the respective case class instance" in {
      b.convertTo[TestTransient] mustEqual obj
    }
  }

  "A BencodingFormat created with `bencodingFormat`, for a case class with 0 elements," should {
    import TestProtocol1._
    val obj = Test0()
    val b = BcDict()
    "convert to a respective BcDict" in {
      obj.toBencoding mustEqual b
    }
    "convert a BcDict to the respective case class instance" in {
      b.convertTo[Test0] mustEqual obj
    }
    "ignore additional members during deserialization" in {
      BcDict("a" -> BcInt(42)).convertTo[Test0] mustEqual obj
    }
    "throw a DeserializationException if the BcValue is not a BcDict" in (
      BcNil.convertTo[Test0] must throwA(new DeserializationException("Object expected"))
    )
  }

  "A BencodingFormat created with `bencodingFormat`, for a case class with mangled-name members," should {
    import TestProtocol1._
    val b = "d8:foo-bar!i42ee"
    "produce the correct bencoding" in {
      TestMangled(42).toBencoding.toString === b
    }
    "convert a JsObject to the respective case class instance" in {
      b.parseBencoding.convertTo[TestMangled] === TestMangled(42)
    }
  }
}
