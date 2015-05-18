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

package com.mongodb.scala.core.collections

import com.mongodb.scala.core.collection.mutable.Document
import org.bson.{ BsonDocument, BsonArray, BsonString, BsonValue }
import org.scalatest.{ FlatSpec, Matchers }

import scala.collection.mutable
import scala.language.postfixOps

class MutableDocumentSpec extends FlatSpec with Matchers {

  val emptyDoc = Document.empty
  val doc: Document = Document("key" -> new BsonString("value"), "key2" -> new BsonString("value2"), "key3" -> new BsonString("value3"))
  val docMap: Map[String, BsonValue] = doc.toMap

  "Document lookups" should "be the same as empty documents" in {
    emptyDoc should equal(Document())
  }

  it should "support get()" in {
    doc.get("key") should equal(Some(new BsonString("value")))
    doc.get("nonexistent") should equal(None)
  }

  it should "support direct lookup" in {
    doc("key") should equal(new BsonString("value"))
    doc[BsonString]("key") should equal(new BsonString("value"))

    // When the key doesn't exist
    an[NoSuchElementException] should be thrownBy doc("nonexistent")

    // When the key exists but the type doesn't match"
    an[NoSuchElementException] should be thrownBy doc[BsonArray]("key")
  }

  it should "support getOrElse" in {
    doc.getOrElse("key", false) should equal(new BsonString("value"))
    doc.getOrElse("nonexistent", false) should equal(false)
  }

  it should "support contains" in {
    doc contains "key" should equal(true)
    doc contains "nonexistent" should equal(false)
  }

  "Document additions and updates" should "support simple additions" in {
    val doc1: Document = emptyDoc + ("key" -> new BsonString("value"))
    emptyDoc should not be doc1
    doc1 should equal(Document("key" -> new BsonString("value")))

    val doc2: Document = doc1 + ("key2" -> new BsonString("value2"))
    doc1 should not be doc2
    doc2 should equal(Document("key" -> new BsonString("value"), "key2" -> new BsonString("value2")))
  }

  it should "support multiple additions" in {
    val doc1: Document = emptyDoc + ("key" -> new BsonString("value"), "key2" -> new BsonString("value2"),
      "key3" -> new BsonString("value3"))
    emptyDoc should not be doc1
    doc1 should equal(Document("key" -> new BsonString("value"), "key2" -> new BsonString("value2"), "key3" -> new BsonString("value3")))

    val doc2: Document = doc1 + ("key4" -> new BsonString("value4"))
    doc1 should not be doc2
    doc2 should equal(Document("key" -> new BsonString("value"), "key2" -> new BsonString("value2"), "key3" -> new BsonString("value3"),
      "key4" -> new BsonString("value4")))
  }

  it should "support addition of a traversable" in {
    val doc1: Document = emptyDoc ++ Set("key" -> new BsonString("value"), "key2" -> new BsonString("value2"))
    emptyDoc should not be doc1
    doc1 should equal(Document("key" -> new BsonString("value"), "key2" -> new BsonString("value2")))

    val doc2: Document = doc1 ++ List("key3" -> new BsonString("value3"))
    doc1 should not be doc2
    doc2 should equal(Document("key" -> new BsonString("value"), "key2" -> new BsonString("value2"), "key3" -> new BsonString("value3")))
  }

  it should "support updated" in {
    val doc1: Document = emptyDoc updated ("key", new BsonString("value"))
    emptyDoc should not be doc1
    doc1 should equal(Document("key" -> new BsonString("value")))

    val doc2: Document = doc1 updated ("key2" -> new BsonString("value2"))
    doc1 should not be doc2
    doc2 should equal(Document("key" -> new BsonString("value"), "key2" -> new BsonString("value2")))
  }

  "Document removals" should "support subtractions" in {
    val doc1: Document = doc - "nonexistent key"
    doc1 should equal(doc)

    val doc2: Document = doc - "key"
    doc1 should not be doc2
    doc2 should equal(Document("key2" -> new BsonString("value2"), "key3" -> new BsonString("value3")))

  }

  it should "support multiple subtractions" in {
    val doc1: Document = doc - ("key", "key2")
    doc should not be doc1
    doc1 should equal(Document("key3" -> new BsonString("value3")))

  }

  it should "support subtraction of a traversable" in {
    val doc1: Document = doc -- Set("key", "key2")
    doc should not be doc1
    doc1 should equal(Document("key3" -> new BsonString("value3")))

    val doc2: Document = doc -- List("key3")
    doc1 should not be doc2
    doc2 should equal(Document("key" -> new BsonString("value"), "key2" -> new BsonString("value2")))

  }

  "Document subcollections" should "provide keys in the order set" in {
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

  "Document transformations" should "be filterable by keys" in {
    val doc1: Document = doc.filterKeys(k => k == "key")

    doc1 should equal(Document("key" -> new BsonString("value")))
  }

  "Traversable helpers" should "work as expected" in {
    val map = mutable.Map[String, BsonValue]()
    doc foreach (kv => map += kv)

    doc.toMap should equal(map)
  }

  it should "be able to create new Documents from iterable" in {
    val doc1 = Document(docMap)
    doc should equal(doc1)
  }

  it should "be mappable thanks to CanBuildFrom" in {
    Document.empty.map({ kv => kv }) should equal(Document.empty)
    val doc1: Document = docMap.map(kv => kv)

    doc1 should equal(doc)
  }

  it should "return a BsonDocument" in {
    val bsonDoc: BsonDocument = doc.toBsonDocument
    doc.underlying should equal(bsonDoc)
  }

  it should "return a Json representation" in {
    doc.toJson should equal("""{ "key" : "value", "key2" : "value2", "key3" : "value3" }""")
  }

  "Documents" should "support Traversable like builders" in {
    val doc1 = doc.filter(kv => kv._1 == "key")

    doc1 should not equal (doc)
    doc1 should equal(Document("key" -> new BsonString("value")))
  }

  "Mutable Documents" should "have maplike mutability" in {
    val doc1 = Document.empty
    doc1 += (("x", new BsonString("x")))

    doc1 should equal(Document("x" -> new BsonString("x")))
  }

  it should "support multiple inline additions" in {
    val doc1: Document = Document.empty += ("key" -> new BsonString("value"), "key2" -> new BsonString("value2"))
    doc1 should equal(Document("key" -> new BsonString("value"), "key2" -> new BsonString("value2")))

    val doc2: Document = doc1 += ("key3" -> new BsonString("value3"))
    doc1 should equal(doc2)
    doc2 should equal(Document("key" -> new BsonString("value"), "key2" -> new BsonString("value2"), "key3" -> new BsonString("value3")))
  }

  it should "support inline addition of a traversable" in {
    val doc1: Document = Document.empty ++= Set("key" -> new BsonString("value"), "key2" -> new BsonString("value2"))
    doc1 should equal(Document("key" -> new BsonString("value"), "key2" -> new BsonString("value2")))

    val doc2: Document = doc1 ++= List("key3" -> new BsonString("value3"))
    doc1 should equal(doc2)
    doc2 should equal(Document("key" -> new BsonString("value"), "key2" -> new BsonString("value2"), "key3" -> new BsonString("value3")))
  }

  it should "support put" in {
    val doc1: Document = Document.empty
    doc1.put("key", new BsonString("value")) shouldBe None
    doc1 should equal(Document("key" -> new BsonString("value")))

    doc1.put("key", new BsonString("newValue")) shouldBe Some(new BsonString("value"))
    doc1 should equal(Document("key" -> new BsonString("newValue")))
  }

  it should "support getOrElseUpdate" in {
    val doc1: Document = Document.empty
    doc1.getOrElseUpdate("key", new BsonString("value")) shouldBe new BsonString("value")
    doc1 should equal(Document("key" -> new BsonString("value")))

    doc1.getOrElseUpdate("key", new BsonString("newValue")) shouldBe new BsonString("value")
    doc1 should equal(Document("key" -> new BsonString("value")))
  }

  it should "support inline update" in {
    val doc1: Document = Document.empty
    doc1 update ("key", new BsonString("value"))
    doc1 should equal(Document("key" -> new BsonString("value")))

    doc1 update ("key2", new BsonString("value2"))
    doc1 should equal(Document("key" -> new BsonString("value"), "key2" -> new BsonString("value2")))
  }

  "Document removals" should "support inline subtractions" in {
    val doc1: Document = doc.copy() -= "nonexistent key"
    doc1 should equal(doc)

    val doc2: Document = doc1 -= "key"
    doc1 should not be equal(doc2)
    doc2 should equal(Document("key2" -> new BsonString("value2"), "key3" -> new BsonString("value3")))
  }

  it should "support multiple inline subtractions" in {
    val doc1: Document = doc.copy() -= ("key", "key2")
    doc should not be doc1
    doc1 should equal(Document("key3" -> new BsonString("value3")))
  }

  it should "support inline subtraction of a traversable" in {
    val doc1: Document = doc.copy() --= Set("key", "key2")
    doc should not be doc1
    doc1 should equal(Document("key3" -> new BsonString("value3")))

    val doc2: Document = doc1 --= List("key3")
    doc1 should equal(doc2)
    doc2 should equal(Document())
  }

  it should "support remove" in {
    val doc1: Document = Document("key" -> new BsonString("value"))

    doc1.remove("key") shouldBe Some(new BsonString("value"))
    doc1 should equal(Document())

    doc1.remove("noKey") shouldBe None
    doc1 should equal(Document())
  }

  it should "support retain" in {
    val doc1: Document = Document("key" -> new BsonString("value"), "key2" -> new BsonString("value2"))

    doc1.retain((k, v) => k == "key")
    doc1 should equal(Document("key" -> new BsonString("value")))
  }

  it should "support clearn" in {
    val doc1: Document = Document("key" -> new BsonString("value"), "key2" -> new BsonString("value2"))

    doc1.clear()
    doc1 should equal(Document())
  }

  it should "support transform" in {
    val doc1: Document = Document("key" -> new BsonString("value"), "key2" -> new BsonString("value2"))

    doc1.transform((k, v) => new BsonString(v.asString().getValue.toUpperCase))
    doc1 should equal(Document("key" -> new BsonString("VALUE"), "key2" -> new BsonString("VALUE2")))
  }
}
