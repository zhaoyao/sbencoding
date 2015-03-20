package sbencoding

import java.util

import org.specs2.mutable.Specification

/**
 * User: zhaoyao
 * Date: 3/19/15
 * Time: 14:35
 */
class CollectionFormatsSpec extends Specification with DefaultBencodingProtocol {
  "The listFormat" should {
    val list = List(1, 2, 3)
    val b = BcList(BcInt(1), BcInt(2), BcInt(3))
    "convert a List[Int] to a BcList of BcInts" in {
      list.toBencoding mustEqual b
    }
    "convert a BcList of BcInts to a List[Int]" in {
      b.convertTo[List[Int]] mustEqual list
    }
  }

  "The arrayFormat" should {
    val array = Array(1, 2, 3)
    val b = BcList(BcInt(1), BcInt(2), BcInt(3))
    "convert an Array[Int] to a BcList of BcInts" in {
      array.toBencoding mustEqual b
    }
    "convert a BcList of BcInts to an Array[Int]" in {
      util.Arrays.equals(b.convertTo[Array[Int]], array) must beTrue
    }
  }

  "The mapFormat" should {
    val map = Map("a" -> 1, "b" -> 2, "c" -> 3)
    val b = BcDict("a" -> BcInt(1), "b" -> BcInt(2), "c" -> BcInt(3))
    "convert a Map[String, Long] to a BcDict" in {
      map.toBencoding mustEqual b
    }
    "be able to convert a BcDict to a Map[String, Long]" in {
      b.convertTo[Map[String, Long]] mustEqual map
    }
    "throw an Exception when trying to serialize a map whose key are not serialized to BcStrings" in {
      Map(1 -> "a").toBencoding must throwA(new SerializationException("Map key must be formatted as BcString, not 'i1e'"))
    }
  }

  "The immutableSetFormat" should {
    val set = Set(1, 2, 3)
    val b = BcList(BcInt(1), BcInt(2), BcInt(3))
    "convert a Set[Int] to a BcList of BcInts" in {
      set.toBencoding mustEqual b
    }
    "convert a BcList of BcInts to a Set[Int]" in {
      b.convertTo[Set[Int]] mustEqual set
    }
  }

  "The indexedSeqFormat" should {
    val seq = collection.IndexedSeq(1, 2, 3)
    val b = BcList(BcInt(1), BcInt(2), BcInt(3))
    "convert a Set[Int] to a BcList of BcInts" in {
      seq.toBencoding mustEqual b
    }
    "convert a BcList of BcInts to a IndexedSeq[Int]" in {
      b.convertTo[collection.IndexedSeq[Int]] mustEqual seq
    }
  }

}
