package sbencoding

import scala.reflect.ClassTag

trait CollectionFormats {

  /**
   * Supplies the BencodingFormat for Lists.
   */
  implicit def listFormat[T: BencodingFormat] = new BencodingFormat[List[T]] {
    def write(list: List[T]) = BcList(list.map(_.toBencoding).toVector)
    def read(value: BcValue): List[T] = value match {
      case BcList(elements) => elements.map(_.convertTo[T])(collection.breakOut)
      case x                => deserializationError("Expected List as BcList, but got " + x.getClass.getSimpleName)
    }
  }

  /**
   * Supplies the BencodingFormat for Arrays.
   */
  implicit def arrayFormat[T: BencodingFormat: ClassTag] = new BencodingFormat[Array[T]] {
    def write(array: Array[T]) = BcList(array.map(_.toBencoding).toVector)
    def read(value: BcValue) = value match {
      case BcList(elements) => elements.map(_.convertTo[T]).toArray[T]
      case BcString(data) if scala.reflect.classTag[T].runtimeClass == classOf[Byte] => data.asInstanceOf[Array[T]]
      case x                => deserializationError("Expected Array as BcList, but got " + x.getClass.getSimpleName)
    }
  }

  /**
   * Supplies the BencodingFormat for Maps. The implicitly available BencodingFormat for the key type K must
   * always write BcStrings, otherwise a [[sbencoding.SerializationException]] will be thrown.
   */
  implicit def mapFormat[K: BencodingFormat, V: BencodingFormat] = new BencodingFormat[Map[K, V]] {
    def write(m: Map[K, V]) = BcDict {
      m.map { field =>
        field._1.toBencoding match {
          case BcString(x) => new String(x, "UTF-8") -> field._2.toBencoding
          case x           => throw new SerializationException("Map key must be formatted as BcString, not '" + x.getClass.getSimpleName + "'")
        }
      }
        // skip None value
        .filterNot(field => field._2 eq BcNil)
    }

    def read(value: BcValue) = value match {
      case x: BcDict => x.fields.map { field =>
        (BcString(field._1.getBytes("UTF-8")).convertTo[K], field._2.convertTo[V])
      }(collection.breakOut)
      case x => deserializationError("Expected Map as BcDict, but got " + x.getClass.getSimpleName)
    }
  }

  import collection.{ immutable => imm }

  implicit def immIterableFormat[T: BencodingFormat] = viaSeq[imm.Iterable[T], T](seq => imm.Iterable(seq: _*))
  implicit def immSeqFormat[T: BencodingFormat] = viaSeq[imm.Seq[T], T](seq => imm.Seq(seq: _*))
  implicit def immIndexedSeqFormat[T: BencodingFormat] = viaSeq[imm.IndexedSeq[T], T](seq => imm.IndexedSeq(seq: _*))
  implicit def immLinearSeqFormat[T: BencodingFormat] = viaSeq[imm.LinearSeq[T], T](seq => imm.LinearSeq(seq: _*))
  implicit def immSetFormat[T: BencodingFormat] = viaSeq[imm.Set[T], T](seq => imm.Set(seq: _*))
  implicit def vectorFormat[T: BencodingFormat] = viaSeq[Vector[T], T](seq => Vector(seq: _*))

  import collection._

  implicit def iterableFormat[T: BencodingFormat] = viaSeq[Iterable[T], T](seq => Iterable(seq: _*))
  implicit def seqFormat[T: BencodingFormat] = viaSeq[Seq[T], T](seq => Seq(seq: _*))
  implicit def indexedSeqFormat[T: BencodingFormat] = viaSeq[IndexedSeq[T], T](seq => IndexedSeq(seq: _*))
  implicit def linearSeqFormat[T: BencodingFormat] = viaSeq[LinearSeq[T], T](seq => LinearSeq(seq: _*))
  implicit def setFormat[T: BencodingFormat] = viaSeq[Set[T], T](seq => Set(seq: _*))

  /**
   * A BencodingFormat construction helper that creates a BencodingFormat for an Iterable type I from a builder function
   * List => I.
   */
  def viaSeq[I <: Iterable[T], T: BencodingFormat](f: imm.Seq[T] => I): BencodingFormat[I] = new BencodingFormat[I] {
    def write(iterable: I) = BcList(iterable.map(_.toBencoding).filterNot(_ eq BcNil).toVector)
    def read(value: BcValue) = value match {
      case BcList(elements) => f(elements.map(_.convertTo[T]))
      case x                => deserializationError("Expected Collection as BcList, but got " + x.getClass.getSimpleName)
    }
  }
}