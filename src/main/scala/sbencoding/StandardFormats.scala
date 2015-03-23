package sbencoding

/**
 * User: zhaoyao
 * Date: 3/17/15
 * Time: 22:14
 */
trait StandardFormats {

  this: AdditionalFormats =>

  private[sbencoding]type BF[T] = BencodingFormat[T] // simple alias for reduced verbosity

  implicit def optionFormat[T: BF] = new OptionFormat[T]

  class OptionFormat[T: BF] extends BF[Option[T]] {
    def write(option: Option[T]) = option match {
      case Some(x) => x.toBencoding
      case None    => BcNil
    }
    def read(value: BcValue) = value match {
      case BcNil => None
      case x     => Some(x.convertTo[T])
    }
  }

  implicit def eitherFormat[A: BF, B: BF] = new BF[Either[A, B]] {
    def write(either: Either[A, B]) = either match {
      case Right(a) => a.toBencoding
      case Left(b)  => b.toBencoding
    }
    def read(value: BcValue) = (value.convertTo(safeReader[A]), value.convertTo(safeReader[B])) match {
      case (Right(a), _: Left[_, _])        => Left(a)
      case (_: Left[_, _], Right(b))        => Right(b)
      case (_: Right[_, _], _: Right[_, _]) => deserializationError("Ambiguous Either value: can be read as both, Left and Right, values")
      case (Left(ea), Left(eb))             => deserializationError("Could not read Either value:\n" + ea + "---------- and ----------\n" + eb)
    }
  }

  implicit def tuple1Format[A: BF] = new BF[Tuple1[A]] {
    def write(t: Tuple1[A]) = t._1.toBencoding
    def read(value: BcValue) = Tuple1(value.convertTo[A])
  }

  implicit def tuple2Format[A: BF, B: BF] = new BencodingFormat[(A, B)] {
    def write(t: (A, B)) = BcList(t._1.toBencoding, t._2.toBencoding)
    def read(value: BcValue) = value match {
      case BcList(Seq(a, b)) => (a.convertTo[A], b.convertTo[B])
      case x                 => deserializationError("Expected Tuple2 as BcList, but got " + x.getClass.getSimpleName)
    }
  }

  implicit def tuple3Format[A: BF, B: BF, C: BF] = new BencodingFormat[(A, B, C)] {
    def write(t: (A, B, C)) = BcList(t._1.toBencoding, t._2.toBencoding, t._3.toBencoding)
    def read(value: BcValue) = value match {
      case BcList(Seq(a, b, c)) => (a.convertTo[A], b.convertTo[B], c.convertTo[C])
      case x                    => deserializationError("Expected Tuple3 as BcList, but got " + x.getClass.getSimpleName)
    }
  }

  implicit def tuple4Format[A: BF, B: BF, C: BF, D: BF] = new BencodingFormat[(A, B, C, D)] {
    def write(t: (A, B, C, D)) = BcList(t._1.toBencoding, t._2.toBencoding, t._3.toBencoding, t._4.toBencoding)
    def read(value: BcValue) = value match {
      case BcList(Seq(a, b, c, d)) => (a.convertTo[A], b.convertTo[B], c.convertTo[C], d.convertTo[D])
      case x                       => deserializationError("Expected Tuple4 as BcList, but got " + x.getClass.getSimpleName)
    }
  }

  implicit def tuple5Format[A: BF, B: BF, C: BF, D: BF, E: BF] = {
    new BencodingFormat[(A, B, C, D, E)] {
      def write(t: (A, B, C, D, E)) = BcList(t._1.toBencoding, t._2.toBencoding, t._3.toBencoding, t._4.toBencoding, t._5.toBencoding)
      def read(value: BcValue) = value match {
        case BcList(Seq(a, b, c, d, e)) =>
          (a.convertTo[A], b.convertTo[B], c.convertTo[C], d.convertTo[D], e.convertTo[E])
        case x => deserializationError("Expected Tuple5 as BcList, but got " + x.getClass.getSimpleName)
      }
    }
  }

  implicit def tuple6Format[A: BF, B: BF, C: BF, D: BF, E: BF, F: BF] = {
    new BencodingFormat[(A, B, C, D, E, F)] {
      def write(t: (A, B, C, D, E, F)) = BcList(t._1.toBencoding, t._2.toBencoding, t._3.toBencoding, t._4.toBencoding, t._5.toBencoding, t._6.toBencoding)
      def read(value: BcValue) = value match {
        case BcList(Seq(a, b, c, d, e, f)) =>
          (a.convertTo[A], b.convertTo[B], c.convertTo[C], d.convertTo[D], e.convertTo[E], f.convertTo[F])
        case x => deserializationError("Expected Tuple6 as BcList, but got " + x.getClass.getSimpleName)
      }
    }
  }

  implicit def tuple7Format[A: BF, B: BF, C: BF, D: BF, E: BF, F: BF, G: BF] = {
    new BencodingFormat[(A, B, C, D, E, F, G)] {
      def write(t: (A, B, C, D, E, F, G)) = BcList(t._1.toBencoding, t._2.toBencoding, t._3.toBencoding, t._4.toBencoding, t._5.toBencoding, t._6.toBencoding, t._7.toBencoding)
      def read(value: BcValue) = value match {
        case BcList(Seq(a, b, c, d, e, f, g)) =>
          (a.convertTo[A], b.convertTo[B], c.convertTo[C], d.convertTo[D], e.convertTo[E], f.convertTo[F], g.convertTo[G])
        case x => deserializationError("Expected Tuple7 as BcList, but got " + x.getClass.getSimpleName)
      }
    }
  }

}
