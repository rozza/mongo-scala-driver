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
package org.mongodb.scala.core.unit

import java.util.Date

import org.bson.BsonString
import org.bson.types.ObjectId
import org.mongodb.scala.core.Implicits._
import org.mongodb.scala.core.collection.Document
import org.mongodb.scala.core.helpers.UnitTestSpec

import scala.language.implicitConversions

class DocumentImplicitSpec extends UnitTestSpec {

  "Document" should "support implicit conversion of strings" in {
    val doc = Document()
    val doc1: Document = doc + ("key" -> "value")
    doc1 should equal(Document("key" -> "value"))

    val doc2: Document = doc updated ("key", "newValue")
    doc2 should equal(Document("key" -> "newValue"))

    val doc3: Document = doc ++ List("key" -> "value", "key2" -> "value2", "key3" -> "value3")
    doc3 should equal(Document("key" -> "value", "key2" -> "value2", "key3" -> "value3"))

    val doc4: Document = doc ++ List(("key", "value"), ("key2", "value2"), ("key3", "value3"))
    doc4 should equal(Document("key" -> "value", "key2" -> "value2", "key3" -> "value3"))
  }

  it should "support implicit conversion of numbers" in {
    val doc = Document()
    val doc1: Document = doc + ("key" -> 1)
    doc1 should equal(Document("key" -> 1))

    val doc2: Document = doc updated ("key", 1L)
    doc2 should equal(Document("key" -> 1L))

    val doc3: Document = doc updated ("key", 2.0)
    doc3 should equal(Document("key" -> 2.0))
  }

  it should "support implicit conversion of booleans" in {
    val doc = Document()
    val doc1: Document = doc + ("key" -> true)
    doc1 should equal(Document("key" -> true))

    val doc2: Document = doc updated ("key", false)
    doc2 should equal(Document("key" -> false))
  }

  it should "support implicit conversion of Date" in {
    val doc = Document()
    val date = new Date()
    val doc1: Document = doc + ("key" -> date)
    doc1 should equal(Document("key" -> date))
  }

  it should "support implicit conversion of ObjectId" in {
    val doc = Document()
    val objectId = new ObjectId()
    val doc1: Document = doc + ("key" -> objectId)
    doc1 should equal(Document("key" -> objectId))
  }

  it should "support implicit conversion of Null" in {
    val doc = Document()
    val doc1: Document = doc + ("key" -> None)
    doc1 should equal(Document("key" -> None))
  }

  it should "support implicit conversion of Symbol" in {
    val doc = Document()
    val doc1: Document = doc + ("key" -> 'symbol)
    doc1 should equal(Document("key" -> 'symbol))
  }

  it should "support mixing in iterables of `(String, Any)`" in {
    val doc = Document()
    val doc1: Document = doc ++ Map("key" -> 1, "key2" -> 2.0, "key3" -> 3L, "key4" -> "4")
    doc1 should equal(Document("key" -> 1, "key2" -> 2.0, "key3" -> 3L, "key4" -> "4"))

    val doc2: Document = doc ++ List("key" -> 1, "key2" -> 2.0, "key3" -> 3L, "key4" -> "4")
    doc2 should equal(Document("key" -> 1, "key2" -> 2.0, "key3" -> 3L, "key4" -> "4"))

    val doc3: Document = doc ++ List(("key", 1), ("key2", 2.0), ("key3", 3L), ("key4", "4"))
    doc3 should equal(Document("key" -> 1, "key2" -> 2.0, "key3" -> 3L, "key4" -> "4"))
  }

  it should "support nesting iterables" in {
    val doc = Document()
    val nestedDoc = Document("key" -> "value", "key2" -> "value2", "key3" -> "value3")

    val doc1 = doc + ("nested" -> nestedDoc)
    doc1 should equal(Document("nested" -> nestedDoc))

    val doc2 = doc + ("nested" -> nestedDoc.values)
    doc2 should equal(Document("nested" -> nestedDoc.values))
  }

  it should "support nesting iterables with mixed types" in {
    val doc = Document()
    val nestedDoc = Document("key" -> "value", "key2" -> 2.0, "key3" -> 3L, "key4" -> false,
      "key5" -> new BsonString("BsonValue"))
    val nestedList = nestedDoc.values

    val doc1 = doc + ("nested" -> nestedDoc)
    doc1 should equal(Document("nested" -> nestedDoc))

    val doc2 = doc + ("nested" -> nestedDoc.values)
    doc2 should equal(Document("nested" -> nestedDoc.values))
  }

  it should "throw a runtime error if it cant convert the type" in {
    val doc = Document()
    an[IllegalArgumentException] should be thrownBy doc + (("key", new Object))
  }

}
