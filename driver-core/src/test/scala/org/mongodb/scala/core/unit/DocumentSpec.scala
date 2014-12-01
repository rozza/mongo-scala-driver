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

import org.bson.BsonString

import scala.collection.Map
import scala.language.{ implicitConversions, reflectiveCalls }

import org.mongodb.scala.core.collection.{ Document, immutable, mutable }
import org.mongodb.scala.core.helpers.UnitTestSpec

class DocumentSpec extends UnitTestSpec {

  def mapValues = Seq(("key", new BsonString("value")),
    ("key2", new BsonString("value2")),
    ("key3", new BsonString("value3")))

  def mutableDocument = mutable.Document(mapValues: _*)
  def immutableDocument = immutable.Document(mapValues: _*)

  def emptyDocuments = Table(("Document", "Instance"), ("immutable", immutable.Document()), ("mutable", mutable.Document()))
  def documents = Table(("Document", "Instance"), ("immutable", immutableDocument), ("mutable", mutableDocument))

  forAll(documents) { (docType, doc) =>
    s"Document ($docType) lookups" should "be the same as empty documents" in {
      val emptyDoc: Document = doc.empty
      emptyDoc should equal(Document())
    }

    it should "support get()" in {
      doc.get("key") should equal(Some(new BsonString("value")))
      doc.get("nonexistent") should equal(None)
    }

    it should "support direct lookup" in {
      doc("key") should equal(new BsonString("value"))

      an[NoSuchElementException] should be thrownBy doc("nonexistent")
    }

    it should "support getOrElse" in {
      doc.getOrElse("key", false) should equal(new BsonString("value"))
      doc.getOrElse("nonexistent", false) should equal(false)
    }

    it should "support contains" in {
      doc contains "key" should equal(true)
      doc contains "nonexistent" should equal(false)
    }

    it should "support isDefinedAt" in {
      doc isDefinedAt "key" should equal(true)
      doc isDefinedAt "nonexistent" should equal(false)
    }
  }

  forAll(emptyDocuments) { (docType, doc) =>
    s"Document ($docType) additions and updates" should "support simple additions" in {
      val doc1: Document = doc + ("key" -> new BsonString("value"))
      doc1 should equal(Document("key" -> new BsonString("value")))

      val doc2: Document = doc1 + ("key2" -> new BsonString("value2"))
      doc2 should equal(Document("key" -> new BsonString("value"), "key2" -> new BsonString("value2")))
    }

    it should "support multiple additions" in {
      val doc1: Document = doc + ("key" -> new BsonString("value"), "key2" -> new BsonString("value2"))
      doc1 should equal(Document("key" -> new BsonString("value"), "key2" -> new BsonString("value2")))

      val doc2: Document = doc1 + ("key3" -> new BsonString("value3"))
      doc2 should equal(Document("key" -> new BsonString("value"), "key2" -> new BsonString("value2"), "key3" -> new BsonString("value3")))
    }

    it should "support addition of a traversable" in {
      val doc1: Document = doc ++ Set("key" -> new BsonString("value"), "key2" -> new BsonString("value2"))
      doc1 should equal(Document("key" -> new BsonString("value"), "key2" -> new BsonString("value2")))

      val doc2: Document = doc1 ++ List("key3" -> new BsonString("value3"))
      doc2 should equal(Document("key" -> new BsonString("value"), "key2" -> new BsonString("value2"), "key3" -> new BsonString("value3")))
    }

    it should "support updated" in {
      val doc1: Document = doc updated ("key", new BsonString("value"))
      doc1 should equal(Document("key" -> new BsonString("value")))

      val doc2: Document = doc1 updated ("key2" -> new BsonString("value2"))
      doc2 should equal(Document("key" -> new BsonString("value"), "key2" -> new BsonString("value2")))
    }
  }

  forAll(documents) { (docType, doc) =>
    s"Document ($docType) removals" should "support subtractions" in {
      val doc1: Document = doc - "nonexistent key"
      doc1 should equal(doc)

      val doc2: Document = doc - "key"
      doc2 should equal(Document("key2" -> new BsonString("value2"), "key3" -> new BsonString("value3")))
    }

    it should "support multiple subtractions" in {
      val doc1: Document = doc - ("key", "key2")
      doc1 should equal(Document("key3" -> new BsonString("value3")))
    }

    it should "support subtraction of a traversable" in {
      val doc1: Document = doc -- Set("key", "key2")
      doc1 should equal(Document("key3" -> new BsonString("value3")))

      val doc2: Document = doc -- List("key3")
      doc2 should equal(Document("key" -> new BsonString("value"), "key2" -> new BsonString("value2")))
    }
  }

  forAll(documents) { (docType, doc) =>
    s"Document ($docType) subcollections" should "provide keys in the order set" in {
      doc.keys should equal(Set("key", "key2", "key3"))

      val doc1: Document = doc + ("aNewKey" -> new BsonString("1"))
      doc1.keys should equal(Set("key", "key2", "key3", "aNewKey"))
    }

    it should "provide a keySet in the order set" in {
      doc.keySet should equal(Set("key", "key2", "key3"))

      val doc1: Document = doc + ("aNewKey" -> new BsonString("1"))
      doc1.keySet should equal(Set("key", "key2", "key3", "aNewKey"))
    }

    it should "provide a keysIterator in the order set" in {
      doc.keysIterator.toSet should equal(Set("key", "key2", "key3"))

      val doc1: Document = doc + ("aNewKey" -> new BsonString("1"))
      doc1.keysIterator.toSet should equal(Set("key", "key2", "key3", "aNewKey"))
    }

    it should "provide values in the order set" in {
      doc.values.toSet should equal(Set(new BsonString("value"), new BsonString("value2"), new BsonString("value3")))

      val doc1: Document = doc + ("aNewKey" -> new BsonString("1"))
      doc1.values.toSet should equal(Set(new BsonString("value"), new BsonString("value2"), new BsonString("value3"), new BsonString("1")))
    }

    it should "provide a valueSet in the order set" in {
      doc.valuesIterator.toSet should equal(Set(new BsonString("value"), new BsonString("value2"), new BsonString("value3")))

      val doc1: Document = doc + ("aNewKey" -> new BsonString("1"))
      doc1.valuesIterator.toSet should equal(Set(new BsonString("value"), new BsonString("value2"), new BsonString("value3"), new BsonString("1")))
    }
  }

  forAll(documents) { (docType, doc) =>
    s"Document ($docType) transformations" should "be filterable by keys" in {
      val doc1: Document = doc.filterKeys(k => k == "key")

      doc1 should equal(Document("key" -> new BsonString("value")))
    }

    it should "be mappable" in {
      val doc1: Map[String, String] = doc.mapValues(v => v.asString().getValue)
      doc1 should equal(Map("key" -> "value", "key2" -> "value2", "key3" -> "value3"))
    }
  }

  "Document (mutable) special cases" should "support lookup updating" in {
    val doc = mutableDocument
    doc("key") = new BsonString("newValue")
    doc should equal(Document("key" -> new BsonString("newValue"), "key2" -> new BsonString("value2"), "key3" -> new BsonString("value3")))

    doc("newKey") = new BsonString("newValue")
    doc should equal(Document("key" -> new BsonString("newValue"), "key2" -> new BsonString("value2"), "key3" -> new BsonString("value3"), "newKey" -> new BsonString("newValue")))
  }

  it should "support in place addition" in {
    val doc = mutableDocument
    doc += (("key", new BsonString("newValue")))
    doc should equal(Document("key" -> new BsonString("newValue"), "key2" -> new BsonString("value2"), "key3" -> new BsonString("value3")))

    doc += (("newKey", new BsonString("newValue")))
    doc should equal(Document("key" -> new BsonString("newValue"), "key2" -> new BsonString("value2"), "key3" -> new BsonString("value3"), "newKey" -> new BsonString("newValue")))
  }

  it should "support multiple in place addition" in {
    val doc = mutableDocument
    doc += (("key", new BsonString("newValue")), ("key2", new BsonString("newValue")))
    doc should equal(Document("key" -> new BsonString("newValue"), "key2" -> new BsonString("newValue"), "key3" -> new BsonString("value3")))

    doc += (("newKey", new BsonString("newValue")), ("newKey2", new BsonString("newValue")))
    doc should equal(Document("key" -> new BsonString("newValue"), "key2" -> new BsonString("newValue"), "key3" -> new BsonString("value3"), "newKey" -> new BsonString("newValue"), "newKey2" -> new BsonString("newValue")))
  }

  it should "support in place addition of a traversable" in {
    val doc = mutableDocument
    doc ++= List(("key", new BsonString("newValue")), ("key2", new BsonString("newValue")))
    doc should equal(Document("key" -> new BsonString("newValue"), "key2" -> new BsonString("newValue"), "key3" -> new BsonString("value3")))

    doc ++= List(("newKey", new BsonString("newValue")), ("newKey2", new BsonString("newValue")))
    doc should equal(Document("key" -> new BsonString("newValue"), "key2" -> new BsonString("newValue"), "key3" -> new BsonString("value3"), "newKey" -> new BsonString("newValue"), "newKey2" -> new BsonString("newValue")))
  }

  it should "support put" in {
    val doc = mutableDocument
    val key = doc put ("key", new BsonString("newValue"))

    key should equal(Some(new BsonString("value")))
    doc should equal(Document("key" -> new BsonString("newValue"), "key2" -> new BsonString("value2"), "key3" -> new BsonString("value3")))
  }

  it should "support getOrElseUpdate" in {
    val doc = mutableDocument
    val key = doc getOrElseUpdate ("key", new BsonString("newValue"))

    key should equal(new BsonString("value"))
    doc should equal(Document("key" -> new BsonString("value"), "key2" -> new BsonString("value2"), "key3" -> new BsonString("value3")))

    val key2 = doc getOrElseUpdate ("newKey", new BsonString("newValue"))
    key2 should equal(new BsonString("newValue"))
    doc should equal(Document("key" -> new BsonString("value"), "key2" -> new BsonString("value2"), "key3" -> new BsonString("value3"), "newKey" -> new BsonString("newValue")))
  }

  it should "support in place subtraction" in {
    val doc = mutableDocument
    doc -= "key"
    doc should equal(Document("key2" -> new BsonString("value2"), "key3" -> new BsonString("value3")))

    doc -= "newKey"
    doc should equal(Document("key2" -> new BsonString("value2"), "key3" -> new BsonString("value3")))
  }

  it should "support multiple in place subtraction" in {
    val doc = mutableDocument
    doc -= ("key", "key2")
    doc should equal(Document("key3" -> new BsonString("value3")))
  }

  it should "support in place subtraction of a traversable" in {
    val doc = mutableDocument
    doc --= List("key", "key2")
    doc should equal(Document("key3" -> new BsonString("value3")))
  }

  it should "support remove" in {
    val doc = mutableDocument
    val key = doc remove "key"

    key should equal(Some(new BsonString("value")))
    doc should equal(Document("key2" -> new BsonString("value2"), "key3" -> new BsonString("value3")))
  }

  it should "support retain" in {
    val doc = mutableDocument
    doc retain ((k, v) => k == "key" | v == new BsonString("value2"))

    doc should equal(Document("key" -> new BsonString("value"), "key2" -> new BsonString("value2")))
  }

  it should "support clear" in {
    val doc = mutableDocument
    doc.clear()

    doc should equal(Document())
  }

  it should "support transform" in {
    val doc = mutableDocument
    doc transform ((k, v) => new BsonString("A"))

    doc should equal(Document("key" -> new BsonString("A"), "key2" -> new BsonString("A"), "key3" -> new BsonString("A")))
  }

  it should "support clone" in {
    val doc = mutableDocument
    val doc2 = doc.clone()

    doc += (("newKey", new BsonString("newValue")))
    doc2.get("newKey") should equal(None)
  }

}
