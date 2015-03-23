package sbencoding

import org.specs2.mutable.Specification

/**
 * User: zhaoyao
 * Date: 3/19/15
 * Time: 14:48
 */
class CustomFormatSpec extends Specification with DefaultBencodingProtocol {
  case class MyType(name: String, value: Int)

  implicit val MyTypeProtocol = new BencodingFormat[MyType] {
    def read(v: BcValue) = {
      v.asBcDict.getFields("name", "value") match {
        case Seq(BcString(name), BcInt(value)) => MyType(new String(name, "UTF-8"), value.toInt)
        case _                                 => deserializationError("Expected fields: 'name' (Bencoding string) and 'value' (Bencoding int)")
      }
    }
    def write(obj: MyType) = BcDict("name" -> BcString(obj.name, "UTF-8"), "value" -> BcInt(obj.value))
  }

  "A custom BencodingFormat built with 'asJsonObject'" should {
    val value = MyType("bob", 42)

    "correctly deserialize valid Bencoding content" in {
      """d4:name3:bob5:valuei42ee""".parseBencoding.convertTo[MyType] mustEqual value
    }

    "support full round-trip (de)serialization" in {
      value.toBencoding.convertTo[MyType] mustEqual value
    }
  }
}
