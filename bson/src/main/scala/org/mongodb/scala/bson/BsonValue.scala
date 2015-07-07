/*
 * Copyright 2015 MongoDB, Inc.
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

package org.mongodb.scala.bson

import java.util.Date

import scala.collection.JavaConverters._
import scala.util.matching.Regex

import org.bson.types.ObjectId
import org.bson.{BsonDocument => JBsonDocument}


/**
 * Companion helper for a BsonArray
 * @since 1.0
 */
object BsonArray {
  import BsonMagnets._

  def apply(): BsonArray = new BsonArray()
  def apply(elems: Iterable[BsonValue]): BsonArray = new BsonArray(elems.toList.asJava)
  def apply(elems: CanBeBsonValue*): BsonArray = new BsonArray(elems.map(_.value).asJava)
}

/**
 * Companion helper for a BsonBinary
 * @since 1.0
 */
object BsonBinary {
  def apply(value: Array[Byte]): BsonBinary = new BsonBinary(value)
}

/**
 * Companion helper for a BsonBoolean
 * @since 1.0
 */
object BsonBoolean {
  def apply(value: Boolean): BsonBoolean = new BsonBoolean(value)
}

/**
 * Companion helper for a BsonDateTime
 * @since 1.0
 */
object BsonDateTime {
  def apply(value: Long): BsonDateTime = new BsonDateTime(value)

  def apply(millis: Date): BsonDateTime = new BsonDateTime(millis.getTime)
}

/**
 * Companion helper for a BsonDocument
 * @since 1.0
 */
object BsonDocument {

  import BsonMagnets._

  def apply(): BsonDocument = new JBsonDocument()

  def apply(elems: Traversable[(String, BsonValue)]): BsonDocument = {
    val bsonDocument = new JBsonDocument()
    elems.foreach(kv => bsonDocument.put(kv._1, kv._2))
    bsonDocument
  }

  def apply(elems: CanBeBsonElement*): BsonDocument = {
    val bsonDocument = new JBsonDocument()
    elems.foreach(elem => bsonDocument.put(elem.key, elem.value))
    bsonDocument
  }

  def apply(json: String): BsonDocument = JBsonDocument.parse(json)
}

/**
 * Companion helper for a BsonDouble
 * @since 1.0
 */
object BsonDouble {
  def apply(value: Double): BsonDouble = new BsonDouble(value)
}

/**
 * Companion helper for a BsonInt32
 * @since 1.0
 */
object BsonInt32 {
  def apply(value: Int): BsonInt32 = new BsonInt32(value)
}

/**
 * Companion helper for a BsonInt64
 * @since 1.0
 */
object BsonInt64 {
  def apply(value: Long): BsonInt64 = new BsonInt64(value)
}

/**
 * Companion helper for a BsonJavaScript
 * @since 1.0
 */
object BsonJavaScript {
  def apply(value: String): BsonJavaScript = new BsonJavaScript(value)
}

/**
 * Companion helper for a BsonJavaScriptWithScope
 * @since 1.0
 */
object BsonJavaScriptWithScope {
  import BsonMagnets._

  def apply(value: String, scope: BsonDocument): BsonJavaScriptWithScope = new BsonJavaScriptWithScope(value, scope)

  def apply(value: String, scope: CanBeBsonElement*): BsonJavaScriptWithScope = new BsonJavaScriptWithScope(value, BsonDocument(scope: _*))

  def apply(value: String, scope: Traversable[(String, BsonValue)]): BsonJavaScriptWithScope = new BsonJavaScriptWithScope(value, BsonDocument(scope))
}

/**
 * Companion helper for a BsonMaxKey
 * @since 1.0
 */
object BsonMaxKey {
  def apply(): BsonMaxKey = new BsonMaxKey()
}

/**
 * Companion helper for a BsonMinKey
 * @since 1.0
 */
object BsonMinKey {
  def apply(): BsonMinKey = new BsonMinKey()
}

/**
 * Companion helper for a BsonNull
 * @since 1.0
 */
object BsonNull {
  def apply(): BsonNull = new BsonNull()
}

/**
 * Companion helper for a BsonNumber
 * @since 1.0
 */
object BsonNumber {
  def apply(value: Int): BsonNumber = new BsonInt32(value)

  def apply(value: Long): BsonNumber = new BsonInt64(value)

  def apply(value: Double): BsonNumber = new BsonDouble(value)
}

/**
 * Companion helper for a BsonObjectId
 * @since 1.0
 */
object BsonObjectId {
  def apply(): BsonObjectId = new BsonObjectId(new ObjectId())
  def apply(value: String): BsonObjectId = new BsonObjectId(new ObjectId(value))
  def apply(value: ObjectId): BsonObjectId = new BsonObjectId(value)
}

/**
 * Companion helper for a BsonRegularExpression
 * @since 1.0
 */
object BsonRegularExpression {
  def apply(value: Regex): BsonRegularExpression = new BsonRegularExpression(value.regex)

  def apply(value: String): BsonRegularExpression = new BsonRegularExpression(value)

  def apply(value: String, options: String): BsonRegularExpression = new BsonRegularExpression(value, options)
}

/**
 * Companion helper for a BsonString
 * @since 1.0
 */
object BsonString {
  def apply(value: String): BsonString = new BsonString(value)
}

/**
 * Companion helper for a BsonSymbol
 * @since 1.0
 */
object BsonSymbol {
  def apply(value: Symbol): BsonSymbol = new BsonSymbol(value.name)
}

/**
 * Companion helper for a BsonTimestamp
 * @since 1.0
 */
object BsonTimestamp {
  def apply(): BsonTimestamp = new BsonTimestamp(0, 0)
  def apply(time: Int, inc: Int): BsonTimestamp = new BsonTimestamp(time, inc)
}

/**
 * Companion helper for a BsonUndefined
 * @since 1.0
 */
object BsonUndefined {
  def apply(): BsonUndefined = new BsonUndefined()
}
