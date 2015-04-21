package sbencoding

/**
 * Provides additional BencodingFormats and helpers
 */
trait AdditionalFormats {

  implicit object BcValueFormat extends BencodingFormat[BcValue] {
    def write(value: BcValue) = value
    def read(value: BcValue) = value
  }

  /**
   * Constructs a BencodingFormat from its two parts, BencodingReader and BencodingWriter.
   */
  def bencodingFormat[T](reader: BencodingReader[T], writer: BencodingWriter[T]) = new BencodingFormat[T] {
    def write(obj: T) = writer.write(obj)
    def read(json: BcValue) = reader.read(json)
  }

  /**
   * Turns a BencodingWriter into a BencodingFormat that throws an UnsupportedOperationException for reads.
   */
  def lift[T](writer: BencodingWriter[T]) = new BencodingFormat[T] {
    def write(obj: T): BcValue = writer.write(obj)
    def read(value: BcValue) =
      throw new UnsupportedOperationException("BencodingReader implementation missing")
  }

  /**
   * Turns a BencodingReader into a BencodingFormat that throws an UnsupportedOperationException for writes.
   */
  def lift[T <: AnyRef](reader: BencodingReader[T]) = new BencodingFormat[T] {
    def write(obj: T): BcValue =
      throw new UnsupportedOperationException("No BencodingWriter[" + obj.getClass + "] available")
    def read(value: BcValue) = reader.read(value)
  }

  /**
   * Lazy wrapper around serialization. Useful when you want to serialize (mutually) recursive structures.
   */
  def lazyFormat[T](format: => BencodingFormat[T]) = new BencodingFormat[T] {
    lazy val delegate = format;
    def write(x: T) = delegate.write(x);
    def read(value: BcValue) = delegate.read(value);
  }

  /**
   * Wraps an existing BencodingReader with Exception protection.
   */
  def safeReader[A: BencodingReader] = new BencodingReader[Either[Exception, A]] {
    def read(json: BcValue) = {
      try {
        Right(json.convertTo[A])
      } catch {
        case e: Exception => Left(e)
      }
    }
  }

}
