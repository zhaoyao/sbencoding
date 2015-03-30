package sbencoding

import java.nio.file.{Paths, Files}

import org.specs2.mutable.Specification

/**
 * User: zhaoyao
 * Date: 3/19/15
 * Time: 13:22
 */
class BencodingParserSpec extends Specification {

  "The BencodingParser" should {

    "parse '0' to BcInt" in {
      BencodingParser("i0e") === BcInt(0)
    }

    "parse 3:xyz to BcString" in {
      BencodingParser("3:xyz") === BcString("xyz", "UTF-8")
    }

    "parse 0: to Empty BcString" in {
      BencodingParser("0:") === BcString(Array.empty[Byte])
    }

    "parse a simple BcDict" in (
      BencodingParser("""d3:keyi42e4:key25:valuee""") ===
      BcDict("key" -> BcInt(42), "key2" -> BcString("value", "UTF-8"))
    )
    "parse a simple BcList" in (
      BencodingParser("""li123ed3:key4:trueee""") ===
      BcList(BcInt(123), BcDict("key" -> BcString("true", "UTF-8")))
    )

    "parse complete torrent metainfo" in {
      val data = Files.readAllBytes(Paths.get("src/test/resources/r.torrent"))

      BencodingParser(data) must beAnInstanceOf[BcDict]
    }

    "parse announce result" in {
      val data = Files.readAllBytes(Paths.get("src/test/resources/announce.bin"))
      BencodingParser(data) must beAnInstanceOf[BcDict]
    }
  }

}
