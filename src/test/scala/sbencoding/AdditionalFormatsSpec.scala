package sbencoding

import org.specs2.mutable.Specification

/**
 * User: zhaoyao
 * Date: 3/19/15
 * Time: 14:42
 */
class AdditionalFormatsSpec extends Specification {
  case class Container[A](inner: Option[A])

  object ReaderProtocol extends DefaultBencodingProtocol {
    implicit def containerReader[T :BencodingFormat] = lift {
      new BencodingReader[Container[T]] {
        def read(value: BcValue) = value match {
          case BcDict(fields) if fields.contains("content") => Container(Some(bencodingReader[T].read(fields("content"))))
          case _ => deserializationError("Unexpected format: " + value.toString)
        }
      }
    }
  }

  object WriterProtocol extends DefaultBencodingProtocol {
    implicit def containerWriter[T :BencodingFormat] = lift {
      new BencodingWriter[Container[T]] {
        def write(obj: Container[T]) = BcDict("content" -> obj.inner.toBencoding)
      }
    }
  }

  "The liftJsonWriter" should {
    val obj = Container(Some(Container(Some(List(1, 2, 3)))))

    "properly write a Container[Container[List[Int]]] to JSON" in {
      import WriterProtocol._
      obj.toBencoding.toString mustEqual """d7:contentd7:contentli1ei2ei3eeee"""
    }

    "properly read a Container[Container[List[Int]]] from JSON" in {
      import ReaderProtocol._
      """d7:contentd7:contentli1ei2ei3eeee""".parseBencoding.convertTo[Container[Container[List[Int]]]] mustEqual obj
    }
  }

  case class Foo(id: Long, name: String, foos: Option[List[Foo]] = None)

  object FooProtocol extends DefaultBencodingProtocol {
    implicit val fooProtocol: BencodingFormat[Foo] = lazyFormat(bencodingFormat(Foo, "id", "name", "foos"))
  }

  "The lazyFormat wrapper" should {
    "enable recursive format definitions" in {
      import FooProtocol._

      println("d4:foosld4:foosld2:idi3e4:name1:cee2:idi2e4:name1:bed2:idi4e4:name1:dee2:idi1e4:name1:ae".toBencoding)
      Foo(1, "a", Some(Foo(2, "b", Some(Foo(3, "c") :: Nil)) :: Foo(4, "d") :: Nil)).toBencoding.toString mustEqual
        """d4:foosld4:foosld2:idi3e4:name1:cee2:idi2e4:name1:bed2:idi4e4:name1:dee2:idi1e4:name1:ae"""
    }
  }
}
