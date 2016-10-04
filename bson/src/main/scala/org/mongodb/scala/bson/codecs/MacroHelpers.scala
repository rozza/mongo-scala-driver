/*
 * Copyright 2016 MongoDB, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.mongodb.scala.bson.codecs

import scala.collection.JavaConverters._

import org.bson.{ Document => JDocument }

import org.mongodb.scala.bson.{ BsonArray, Document }

object MacroHelpers {

  def toJava(x: Any): Any = {
    x match {
      case y: scala.collection.MapLike[_, _, _] => y.map({ case (d, v) => toJava(d) -> toJava(v) }).asJava
      case y: scala.collection.SetLike[_, _] => y.map(toJava).asJava
      case y: Iterable[_] => y.map(toJava).asJava
      case y: Iterator[_] => toJava(y.toIterable)
      case _ => x
    }
  }

  def toScala(x: Any): Any = {
    x match {
      case y: java.lang.Iterable[_] => y.asScala.map(toScala)
      case d: JDocument => d
      case x: java.util.Map[_, _] => x.asScala.toMap
      case _ => x
    }
  }

  def toOption[T](value: JDocument, converter: JDocument => T): Option[T] = Option(value).map(converter)

  def nestedToJava[T](x: Iterable[_], depth: Int, converter: T => JDocument): Any = {
    depth match {
      case 0 => x.map({ y => converter(y.asInstanceOf[T]) }).asJava
      case _ => x.map({ y => nestedToJava(y.asInstanceOf[Iterable[_]], depth - 1, converter) }).asJava
    }
  }

  def nestedJavaToScala[T](x: java.util.List[_], depth: Int, converter: JDocument => T): Iterable[Any] = {
    depth match {
      case 0 => x.asScala.map(y => converter(y.asInstanceOf[JDocument]))
      case _ => x.asScala.map(y => nestedJavaToScala(y.asInstanceOf[java.util.List[_]], depth - 1, converter))
    }
  }

}
