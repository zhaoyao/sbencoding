package sbencoding

import org.specs2.mutable.Specification

/**
 * User: zhaoyao
 * Date: 3/19/15
 * Time: 14:56
 */
class StandardFormatsSpec extends Specification with DefaultBencodingProtocol {

  "The optionFormat" should {
    "convert None to BcNil" in {
      None.asInstanceOf[Option[Int]].toBencoding mustEqual BcNil
    }
    "convert BcNil to None" in {
      BcNil.convertTo[Option[Int]] mustEqual None
    }
    "convert Some(Hello) to BcString(Hello)" in {
      Some("Hello").asInstanceOf[Option[String]].toBencoding mustEqual BcString("Hello", "UTF-8")
    }
    "convert BcString(Hello) to Some(Hello)" in {
      BcString("Hello", "UTF-8").convertTo[Option[String]] mustEqual Some("Hello")
    }
  }

  "The eitherFormat" should {
    val a: Either[Int, String] = Left(42)
    val b: Either[Int, String] = Right("Hello")

    "convert the left side of an Either value to Json" in {
      a.toBencoding mustEqual BcInt(42)
    }
    "convert the right side of an Either value to Json" in {
      b.toBencoding mustEqual BcString("Hello", "UTF-8")
    }
    "convert the left side of an Either value from Json" in {
      BcInt(42).convertTo[Either[Int, String]] mustEqual Left(42)
    }
    "convert the right side of an Either value from Json" in {
      BcString("Hello", "UTF-8").convertTo[Either[Int, String]] mustEqual Right("Hello")
    }
  }

  "The tuple1Format" should {
    "convert (42) to a BcInt" in {
      Tuple1(42).toBencoding mustEqual BcInt(42)
    }
    "be able to convert a BcInt to a Tuple1[Int]" in {
      BcInt(42).convertTo[Tuple1[Int]] mustEqual Tuple1(42)
    }
  }

  "The tuple2Format" should {
    val b = BcList(BcInt(42), BcInt(42))
    "convert (42, 42) to a BcList" in {
      (42, 42).toBencoding mustEqual b
    }
    "be able to convert a BcList to a (Int, Int)]" in {
      b.convertTo[(Int, Int)] mustEqual ((42, 42))
    }
  }

  "The tuple3Format" should {
    val b = BcList(BcInt(42), BcInt(42), BcInt(3))
    "convert (42, 42, 3) to a BcList" in {
      (42, 42, 3).toBencoding mustEqual b
    }
    "be able to convert a BcList to a (Int, Int, Int)]" in {
      b.convertTo[(Int, Int, Int)] mustEqual ((42, 42, 3))
    }
  }
  "The tuple4Format" should {
    val b = BcList(BcInt(42), BcInt(42), BcInt(3), BcInt(4))
    "convert (42, 42, 3, 4) to a BcList" in {
      (42, 42, 3, 4).toBencoding mustEqual b
    }
    "be able to convert a BcList to a (Int, Int, Int, Int)]" in {
      b.convertTo[(Int, Int, Int, Int)] mustEqual ((42, 42, 3, 4))
    }
  }
  "The tuple5Format" should {
    val b = BcList(BcInt(42), BcInt(42), BcInt(3), BcInt(4), BcInt(5))
    "convert (42, 42, 3, 4, 5) to a BcList" in {
      (42, 42, 3, 4, 5).toBencoding mustEqual b
    }
    "be able to convert a BcList to a (Int, Int, Int, Int, Int)]" in {
      b.convertTo[(Int, Int, Int, Int, Int)] mustEqual ((42, 42, 3, 4, 5))
    }
  }
  "The tuple6Format" should {
    val b = BcList(BcInt(42), BcInt(42), BcInt(3), BcInt(4), BcInt(5), BcInt(6))
    "convert (42, 42, 3, 4, 5, 6) to a BcList" in {
      (42, 42, 3, 4, 5, 6).toBencoding mustEqual b
    }
    "be able to convert a BcList to a (Int, Int, Int, Int, Int, Int)]" in {
      b.convertTo[(Int, Int, Int, Int, Int, Int)] mustEqual ((42, 42, 3, 4, 5, 6))
    }
  }
  "The tuple7Format" should {
    val b = BcList(BcInt(42), BcInt(42), BcInt(3), BcInt(4), BcInt(5), BcInt(6), BcInt(7))
    "convert (42, 42, 3, 4, 5, 6, 7) to a BcList" in {
      (42, 42, 3, 4, 5, 6, 7).toBencoding mustEqual b
    }
    "be able to convert a BcList to a (Int, Int, Int, Int, Int, Int, Int)]" in {
      b.convertTo[(Int, Int, Int, Int, Int, Int, Int)] mustEqual ((42, 42, 3, 4, 5, 6, 7))
    }
  }
}
