/*
 * Original implementation (C) 2009-2011 Debasish Ghosh
 * Adapted and extended in 2011 by Mathias Doenitz
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package sbencoding

import annotation.implicitNotFound

/**
 * Provides the Bencoding deserialization for type T.
 */
@implicitNotFound(msg = "Cannot find BencodingReader or BencodingFormat type class for ${T}")
trait BencodingReader[T] {
  def read(Bencoding: BcValue): T
}

object BencodingReader {
  implicit def func2Reader[T](f: BcValue => T): BencodingReader[T] = new BencodingReader[T] {
    def read(Bencoding: BcValue) = f(Bencoding)
  }
}

/**
 * Provides the Bencoding serialization for type T.
 */
@implicitNotFound(msg = "Cannot find BencodingWriter or BencodingFormat type class for ${T}")
trait BencodingWriter[T] {
  def write(obj: T): BcValue
}

object BencodingWriter {
  implicit def func2Writer[T](f: T => BcValue): BencodingWriter[T] = new BencodingWriter[T] {
    def write(obj: T) = f(obj)
  }
}

/**
 * Provides the Bencoding deserialization and serialization for type T.
 */
trait BencodingFormat[T] extends BencodingReader[T] with BencodingWriter[T]