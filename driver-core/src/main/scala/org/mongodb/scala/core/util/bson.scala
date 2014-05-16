/**
 * Copyright (c) 2014 MongoDB, Inc.
 *
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *
 * For questions and comments about this product, please see the project page at:
 *
 * https://github.com/mongodb/mongo-scala-driver
 */
package org.mongodb.scala.core.util

import java.util.Date

import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}
import scala.util.matching.Regex
import scala.util.parsing.json.JSONFormat
import org.bson.types.ObjectId

object bson {

  val formatter = JSONFormat.defaultFormatter

  trait BsonValue extends Any

  case class BsonObjectId(value: ObjectId) extends AnyVal with BsonValue {
    def toBson = this
    override def toString = s"$value"
  }

  case class BsonString(value: String) extends AnyVal with BsonValue  {
    def toBson = this
    override def toString = s""""$value""""
  }

  case class BsonInt(value: Int) extends AnyVal with BsonValue {
    def toBson = this
    override def toString = s"$value"
  }

  case class BsonLong(value: Long) extends AnyVal with BsonValue {
    def toBson = this
    override def toString = s"$value"
  }

  case class BsonDouble(value: Double) extends AnyVal with BsonValue {
    def toBson = this
    override def toString = s"$value"
  }

  case class BsonDate(value: Date) extends AnyVal with BsonValue  {
    def toBson = this
    override def toString = s"$value"
  }

  case class BsonBoolean(value: Boolean) extends AnyVal with BsonValue {
    def toBson = this
    override def toString = s"$value"
  }

  case class BsonRegex(value: Regex) extends AnyVal with BsonValue {
    def toBson = this
    override def toString = s"$value"
  }

  case class BsonSymbol(value: Symbol) extends AnyVal with BsonValue {
    def toBson = this
    override def toString = s"$value"
  }

  case class BsonBinary(value: Array[Byte], subType: Int) extends BsonValue  {
    def toBson = this
    override def toString = s"$value"
  }

  case class BsonArray(value: List[BsonValue]) extends AnyVal with BsonValue {
    def toBson = this
    override def toString = "["+ value.map({ v => s"$v" }).mkString(", ") +"]"
  }

  case class BsonDocument(value: Map[String, BsonValue]) extends AnyVal with BsonValue {
    def toBson = this
    override def toString = "{"+ value.map({ case (k, v) => s""""$k": $v""" }).mkString(", ") +"}"
  }

  implicit def fromObjectId(value : ObjectId): BsonObjectId = BsonObjectId(value)
  implicit def fromString(value : String): BsonString = BsonString(value)
  implicit def fromInt(value : Int): BsonInt = BsonInt(value)
  implicit def fromLong(value : Long): BsonLong = BsonLong(value)
  implicit def fromDouble(value : Double): BsonDouble = BsonDouble(value)
  implicit def fromDate(value : Date): BsonDate = BsonDate(value)
  implicit def fromBoolean(value : Boolean): BsonBoolean = BsonBoolean(value)
  implicit def fromRegex(value : Regex): BsonRegex = BsonRegex(value)
  implicit def fromSymbol(value : Symbol): BsonSymbol = BsonSymbol(value)

  implicit def fromArray[T](xs: List[T]): BsonArray = BsonArray(xs.map { v =>
    v match {
      case x @ (z: ObjectId)    => z.toBson
      case x @ (z: String)    => z.toBson
      case x @ (z: Int)       => z.toBson
      case x @ (z: Long)      => z.toBson
      case x @ (z: Double)    => z.toBson
      case x @ (z: Date)      => z.toBson
      case x @ (z: Boolean)   => z.toBson
      case x @ (z: Regex)     => z.toBson
      case x @ (z: Symbol)    => z.toBson
      case x @ (z: List[_])  => fromArray(z)
      case x @ (z: Map[_, _]) => {
        Try(z.asInstanceOf[Map[String, _]]) match {
          case Success(z1) => fromMap(z1)
          case Failure(e)  => throw new IllegalArgumentException("Invalid type not a BsonValue: " + v)
        }
      }
      case _ => throw new IllegalArgumentException("Invalid type not a BsonValue: " + v)
    }
  })

  implicit def fromMap[T](xs: Map[String, T]): BsonDocument = BsonDocument(xs.map { case (k, v) =>
    (k, v) match {
      case x @ (_: String, z: ObjectId)   => (k, z.toBson)
      case x @ (_: String, z: String)   => (k, z.toBson)
      case x @ (_: String, z: Int)      => (k, z.toBson)
      case x @ (_: String, z: Long)     => (k, z.toBson)
      case x @ (_: String, z: Double)   => (k, z.toBson)
      case x @ (_: String, z: Date)     => (k, z.toBson)
      case x @ (_: String, z: Boolean)  => (k, z.toBson)
      case x @ (_: String, z: Regex)    => (k, z.toBson)
      case x @ (_: String, z: Symbol)   => (k, z.toBson)
      case x @ (_: String, z: List[_]) => (k, fromArray(z))
      case x @ (_: String, z: Map[_,_]) => {
        Try(z.asInstanceOf[Map[String, _]]) match {
          case Success(z1) => (k, fromMap(z1))
          case Failure(e)  => throw new IllegalArgumentException("Invalid type not a BsonValue: " + v)
        }
      }
      case _ => throw new IllegalArgumentException("Invalid type not a BsonValue: " + v)
    }
  })

  def parse(s: String): BsonDocument = {
    BsonParser.parse(s) getOrElse (throw new IllegalArgumentException("Could not convert to BsonDocument"))
  }

  implicit class BsonStringContext(val sc: StringContext) extends AnyVal {
    def bson(args: Any*) = parse(sc.s(args : _*)).toBson
  }
}
