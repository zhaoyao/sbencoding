package sbencoding

/**
  * Provides additional BencodingFormats and helpers
 */
trait AdditionalFormats {

  implicit object BcValueFormat extends BencodingFormat[BcValue] {
    def write(value: BcValue) = value
    def read(value: BcValue) = value
  }

  implicit object RootBcDictFormat extends RootBencodingFormat[BcDict] {
    def write(value: BcDict) = value
    def read(value: BcValue) = value.asBcDict
  }

  implicit object RootBcListFormat extends RootBencodingFormat[BcList] {
    def write(value: BcList) = value
    def read(value: BcValue) = value match {
      case x: BcList => x
      case _ => deserializationError("JSON array expected")
    }
  }

  /**
   * Constructs a BencodingFormat from its two parts, BencodingReader and BencodingWriter.
   */
  def BencodingFormat[T](reader: BencodingReader[T], writer: BencodingWriter[T]) = new BencodingFormat[T] {
    def write(obj: T) = writer.write(obj)
    def read(json: BcValue) = reader.read(json)
  }

  /**
   * Constructs a RootBencodingFormat from its two parts, RootBencodingReader and RootBencodingWriter.
   */
  def rootBencodingFormat[T](reader: RootBencodingReader[T], writer: RootBencodingWriter[T]) =
    rootFormat(BencodingFormat(reader, writer))

  /**
   * Turns a BencodingWriter into a BencodingFormat that throws an UnsupportedOperationException for reads.
   */
  def lift[T](writer :BencodingWriter[T]) = new BencodingFormat[T] {
    def write(obj: T): BcValue = writer.write(obj)
    def read(value: BcValue) =
      throw new UnsupportedOperationException("BencodingReader implementation missing")
  }

  /**
   * Turns a RootBencodingWriter into a RootBencodingFormat that throws an UnsupportedOperationException for reads.
   */
  def lift[T](writer :RootBencodingWriter[T]): RootBencodingFormat[T] =
    rootFormat(lift(writer :BencodingWriter[T]))

  /**
   * Turns a BencodingReader into a BencodingFormat that throws an UnsupportedOperationException for writes.
   */
  def lift[T <: AnyRef](reader :BencodingReader[T]) = new BencodingFormat[T] {
    def write(obj: T): BcValue =
      throw new UnsupportedOperationException("No BencodingWriter[" + obj.getClass + "] available")
    def read(value: BcValue) = reader.read(value)
  }

  /**
   * Turns a RootBencodingReader into a RootBencodingFormat that throws an UnsupportedOperationException for writes.
   */
  def lift[T <: AnyRef](reader :RootBencodingReader[T]): RootBencodingFormat[T] =
    rootFormat(lift(reader :BencodingReader[T]))

  /**
   * Lazy wrapper around serialization. Useful when you want to serialize (mutually) recursive structures.
   */
  def lazyFormat[T](format: => BencodingFormat[T]) = new BencodingFormat[T] {
    lazy val delegate = format;
    def write(x: T) = delegate.write(x);
    def read(value: BcValue) = delegate.read(value);
  }

  /**
   * Explicitly turns a BencodingFormat into a RootBencodingFormat.
   */
  def rootFormat[T](format: BencodingFormat[T]) = new RootBencodingFormat[T] {
    def write(obj: T) = format.write(obj)
    def read(json: BcValue) = format.read(json)
  }

  /**
   * Wraps an existing BencodingReader with Exception protection.
   */
  def safeReader[A :BencodingReader] = new BencodingReader[Either[Exception, A]] {
    def read(json: BcValue) = {
      try {
        Right(json.convertTo[A])
      } catch {
        case e: Exception => Left(e)
      }
    }
  }

}
