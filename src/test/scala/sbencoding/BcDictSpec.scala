package sbencoding

import org.specs2.mutable.Specification

/**
 * User: zhaoyao
 * Date: 3/20/15
 * Time: 13:53
 */
class BcDictSpec extends Specification {

  "A BcDict" should {

    "getFields return values when key exists" in {

      BcDict("a" -> BcInt(1), "b" -> BcInt(2)).getFields("a", "b") mustEqual Seq(BcInt(1), BcInt(2))

    }

    "getFields return BcNil when key not exists" in {

      BcDict("a" -> BcInt(1), "b" -> BcInt(2)).getFields("a", "b", "c") mustEqual Seq(BcInt(1), BcInt(2), BcNil)

    }

  }

}
