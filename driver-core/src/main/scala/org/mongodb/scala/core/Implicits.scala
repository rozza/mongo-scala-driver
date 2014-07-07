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
package org.mongodb.scala.core

import java.util.Date

import org.bson._
import org.bson.types.ObjectId
import org.mongodb.scala.core.collection.Document

import scala.collection.JavaConverters._
import scala.language.implicitConversions

object Implicits {

  implicit def documentToBsonDocument(value: Document): BsonDocument = value.underlying

  implicit def bsonDocumentToDocument(value: BsonDocument): Document = Document(value)

  implicit def iterableToBsonArray(value: Iterable[BsonValue]): BsonArray = new BsonArray(value.toList.asJava)

  implicit def bsonArrayToIterable(value: BsonArray): Iterable[BsonValue] = value.asScala

  implicit def stringToBsonString(value: String): BsonString = new BsonString(value)

  implicit def bsonStringToString(value: BsonString): String = value.getValue

  implicit def doubleToBsonDouble(value: Double): BsonDouble = new BsonDouble(value)

  implicit def bsonDoubleToDouble(value: BsonDouble): Double = value.doubleValue()

  implicit def intToBsonInt32(value: Int): BsonInt32 = new BsonInt32(value)

  implicit def bsonInt32ToInt(value: BsonInt32): Int = value.intValue()

  implicit def longToBsonInt64(value: Long): BsonInt64 = new BsonInt64(value)

  implicit def bsonInt64ToLong(value: BsonInt64): Long = value.intValue()

  implicit def booleanToBsonBoolean(value: Boolean): BsonBoolean = new BsonBoolean(value)

  implicit def bsonBooleanToBoolean(value: BsonBoolean): Boolean = value.getValue

  implicit def dateTimeToBsonDateTime(value: Date): BsonDateTime = new BsonDateTime(value.getTime)

  implicit def bsonDateTimeToDate(value: BsonDateTime): Date = new Date(value.getValue)

  implicit def objectIdToBsonObjectId(value: ObjectId): BsonObjectId = new BsonObjectId(value)

  implicit def bsonObjectIdToObjectId(value: BsonObjectId): ObjectId = value.getValue

  implicit def noneToBsonNull(value: Option[Nothing]): BsonNull = new BsonNull()

  implicit def nullToBsonNull(value: Null): BsonNull = new BsonNull()

  implicit def bsonNullToNone(value: BsonNull) = None

  implicit def symbolToBsonSymbol(value: Symbol): BsonSymbol = new BsonSymbol(value.toString())

  implicit def bsonSymbolToSymbol(value: BsonSymbol): Symbol = Symbol(value.getSymbol)

  implicit def anyToBsonValue(v: Any): BsonValue = {
    v match {
      case x @ (v: BsonValue) => v
      case x @ (v: String)    => new BsonString(v)
      case x @ (v: Boolean)   => new BsonBoolean(v)
      case x @ (v: Date)      => new BsonDateTime(v.getTime)
      case x @ (v: Document)  => v.underlying
      case x @ (v: Double)    => new BsonDouble(v)
      case x @ (v: Int)       => new BsonInt32(v)
      case x @ (v: Long)      => new BsonInt64(v)
      case x @ (v: ObjectId)  => new BsonObjectId(v)
      case x @ (v: Symbol)    => new BsonSymbol(v.toString())
      case None               => new BsonNull
    }
  }

}

