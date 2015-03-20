/*
 * Copyright (C) 2009-2011 Mathias Doenitz
 * Inspired by a similar implementation by Nathan Hamblen
 * (https://github.com/n8han/Databinder-Dispatch)
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

import java.util

import collection.immutable

/**
 * The general type of a Bencoding AST node.
 */
sealed abstract class BcValue {
  override def toString = toString((x) => new String(BencodingPrinter(x)))
  def toString(printer: (BcValue => String)) = printer(this)
  def convertTo[T: BencodingReader]: T = bencodingReader[T].read(this)

  /**
   * Returns `this` if this JsValue is a JsObject, otherwise throws a DeserializationException with the given error msg.
   */
  def asBcDict(errorMsg: String = "Bencoding dict expected"): BcDict = deserializationError(errorMsg)

  /**
   * Returns `this` if this JsValue is a JsObject, otherwise throws a DeserializationException.
   */
  def asBcDict: BcDict = asBcDict()

}

/**
 * A Bencoding object.
 */
case class BcDict(fields: Map[String, BcValue]) extends BcValue {
  override def asBcDict(errorMsg: String) = this
  def getFields(fieldNames: String*): immutable.Seq[BcValue] = fieldNames.flatMap(fields.get)(collection.breakOut)
}
object BcDict {
  def apply(members: BcField*) = new BcDict(Map(members: _*))
}

/**
 * A Bencoding array.
 */
case class BcList(elements: Vector[BcValue]) extends BcValue
object BcList {
  def apply(elements: BcValue*) = new BcList(elements.toVector)
}

/**
 * A Bencoding string.
 */
case class BcString(value: Array[Byte]) extends BcValue {
  override def equals(obj: scala.Any): Boolean = obj match {
    case BcString(d) => util.Arrays.equals(this.value, d)
    case _           => false
  }
}

object BcString {
  def apply(value: Symbol): BcString = BcString(value.name.getBytes("ascii"))
  def apply(value: String, charset: String): BcString = BcString(value.getBytes(charset))
}

/**
 * A Bencoding number.
 */
case class BcInt(value: Long) extends BcValue

case object BcNil extends BcValue