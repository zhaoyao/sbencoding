/*
 * Copyright (C) 2011 Mathias Doenitz
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

import java.lang.reflect.Modifier
import scala.reflect.ClassTag
import scala.util.control.NonFatal

/**
 * Provides the helpers for constructing custom JsonFormat implementations for types implementing the Product trait
 * (especially case classes)
 */
trait ProductFormats extends ProductFormatsInstances {
  this: StandardFormats =>

  def bencodingFormat0[T](construct: () => T): BencodingFormat[T] =
    new BencodingFormat[T] {
      def write(p: T) = BcDict()
      def read(value: BcValue) = value match {
        case BcDict(_) => construct()
        case _         => throw new DeserializationException("Object expected")
      }
    }

  // helpers

  protected def productElement2Field[T](fieldName: String, p: Product, ix: Int, rest: List[BcField] = Nil)(implicit writer: BencodingWriter[T]): List[BcField] = {
    val value = p.productElement(ix).asInstanceOf[T]
    writer match {
      case _: OptionFormat[_] if (value == None) => rest
      case _                                     => (fieldName, writer.write(value)) :: rest
    }
  }

  protected def fromField[T](value: BcValue, fieldName: String)(implicit reader: BencodingReader[T]) = {
    value match {
      case x: BcDict =>
        var fieldFound = false
        try {
          val fieldValue = x.fields(fieldName)
          fieldFound = true
          reader.read(fieldValue)
        } catch {
          case e: NoSuchElementException if !fieldFound =>
            if (reader.isInstanceOf[OptionFormat[_]]) None.asInstanceOf[T]
            else deserializationError("Object is missing required member '" + fieldName + "'", e)
          case e: DeserializationException =>
            deserializationError("Unable to deserialize member '" + fieldName + "'", e)
        }
      case _ => deserializationError("Object expected in field '" + fieldName + "'")
    }
  }

  protected def extractFieldNames(classTag: ClassTag[_]): Array[String] = {
    val clazz = classTag.runtimeClass
    try {
      // copy methods have the form copy$default$N(), we need to sort them in order, but must account for the fact
      // that lexical sorting of ...8(), ...9(), ...10() is not correct, so we extract N and sort by N.toInt
      val copyDefaultMethods = clazz.getMethods.filter(_.getName.startsWith("copy$default$")).sortBy(
        _.getName.drop("copy$default$".length).takeWhile(_ != '(').toInt)
      val fields = clazz.getDeclaredFields.filterNot { f =>
        f.getName.startsWith("$") || Modifier.isTransient(f.getModifiers) || Modifier.isStatic(f.getModifiers)
      }
      if (copyDefaultMethods.length != fields.length)
        sys.error("Case class " + clazz.getName + " declares additional fields")
      if (fields.zip(copyDefaultMethods).exists { case (f, m) => f.getType != m.getReturnType })
        sys.error("Cannot determine field order of case class " + clazz.getName)
      fields.map(f => ProductFormats.unmangle(f.getName))
    } catch {
      case NonFatal(ex) => throw new RuntimeException("Cannot automatically determine case class field names and order " +
        "for '" + clazz.getName + "', please use the 'bencodingFormat' overload with explicit field name specification", ex)
    }
  }

}

object ProductFormats {
  private val operators = Map(
    "$eq" -> "=",
    "$greater" -> ">",
    "$less" -> "<",
    "$plus" -> "+",
    "$minus" -> "-",
    "$times" -> "*",
    "$div" -> "/",
    "$bang" -> "!",
    "$at" -> "@",
    "$hash" -> "#",
    "$percent" -> "%",
    "$up" -> "^",
    "$amp" -> "&",
    "$tilde" -> "~",
    "$qmark" -> "?",
    "$bar" -> "|")

  private def unmangle(name: String) = operators.foldLeft(name) {
    case (n, (mangled, unmangled)) =>
      if (n.indexOf(mangled) >= 0) n.replace(mangled, unmangled) else n
  }
}