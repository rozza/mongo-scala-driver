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
package org.mongodb.scala.core.collection

import org.bson.{ BsonDocument, BsonValue }

import scala.collection.GenTraversableOnce
import scala.collection.JavaConverters._

trait Document extends scala.collection.Map[String, BsonValue] with DocumentLowPriorityOperations {

  self =>

  override def stringPrefix: String = "Document"
  override def empty: Document = apply(new BsonDocument)

  val underlying: BsonDocument

  def apply(underlying: BsonDocument): Document

  /**
   * Creates a new document containing a new key/value and all the existing key/values.
   *
   *  Mapping `kv` will override existing mappings from this document with the same key.
   *
   *  Uses a view bound so we can treat `B` as a `BsonValue` - may require implicit conversion to a
   *  `BsonValue`
   *
   *  @param kv    the key/value mapping to be added
   *  @return      a new document containing mappings of this document and the mapping `kv`.
   */
  def +[B](kv: (String, B))(implicit ev: B => BsonValue): Document = {
    val bsonDocument: BsonDocument = copyBsonDocument()
    bsonDocument.put(kv._1, kv._2)
    apply(bsonDocument)
  }

  /**
   * Adds two or more elements to this collection and returns
   *  a new collection.
   *
   *  @param elem1 the first element to add.
   *  @param elem2 the second element to add.
   *  @param elems the remaining elements to add.
   *  @return A new document with the new bindings added.
   */
  def +[B](elem1: (String, B), elem2: (String, B), elems: (String, B)*)(implicit ev: B => BsonValue): Document = {
    val bsonDocument: BsonDocument = copyBsonDocument()
    bsonDocument.put(elem1._1, elem1._2)
    bsonDocument.put(elem2._1, elem2._2)
    elems.foreach(kv => bsonDocument.put(kv._1, kv._2))
    apply(bsonDocument)
  }

  /**
   * Adds a number of elements provided by a traversable object
   *  and returns a new collection with the added elements.
   *
   *  @param xs      the traversable object consisting of key-value pairs.
   *  @return        a new immutable document with the bindings of this document and those from `xs`.
   */
  def ++[B](xs: GenTraversableOnce[(String, B)])(implicit ev: B => BsonValue): Document = {
    val bsonDocument = copyBsonDocument()
    xs.foreach(kv => bsonDocument.put(kv._1, kv._2))
    apply(bsonDocument)
  }

  /**
   * Removes a key from this document, returning a new document.
   *  @param    key the key to be removed
   *  @return   a new document without a binding for `key`
   */
  def -(key: String): Document = {
    val newUnderlying = new BsonDocument()
    for ((k, v) <- iterator if k != key) { newUnderlying.put(k, v) }
    apply(newUnderlying)
  }

  /**
   * Creates a new document from this document with some elements removed.
   *
   *  This method takes two or more elements to be removed. Another overloaded
   *  variant of this method handles the case where a single element is
   *  removed.
   *  @param elem1 the first element to remove.
   *  @param elem2 the second element to remove.
   *  @param elems the remaining elements to remove.
   *  @return a new $coll that contains all elements of the current $coll
   *  except one less occurrence of each of the given elements.
   */
  override def -(elem1: String, elem2: String, elems: String*): Document = this -- (List(elem1, elem2) ++ elems)

  /**
   * Creates a new document from this document by removing all elements of another collection.
   *
   *  @param xs the collection containing the removed elements.
   *  @return a new Document that contains all elements of the current document
   *  except one less occurrence of each of the elements of `elems`.
   */
  override def --(xs: GenTraversableOnce[String]): Document = {
    val keysToIgnore = xs.toList
    val newUnderlying = new BsonDocument()
    for ((k, v) <- iterator if !keysToIgnore.contains(k)) { newUnderlying.put(k, v) }
    apply(newUnderlying)
  }

  /**
   *  Creates a new Document consisting of all key/value pairs of the current document
   *  plus a new pair of a given key and value.
   *
   *  @param key    The key to add
   *  @param value  The new value
   *  @return       A fresh immutable document with the binding from `key` to `value` added to the new document.
   */
  def updated[B](key: String, value: B)(implicit ev: B => BsonValue): Document = this + ((key, value))

  /**
   *  Creates a new Document consisting of all key/value pairs of the current document
   *  plus a new pair of a given key and value.
   *
   *  @param kv    The key/value to add
   *  @return       A fresh immutable document with the binding from `key` to `value` added to the new document.
   */
  def updated[B](kv: (String, B))(implicit ev: B => BsonValue): Document = this + kv

  /**
   * Optionally returns the value associated with a key.
   *
   *  @param  key    the key value
   *  @return an option value containing the value associated with `key` in this document,
   *          or `None` if none exists.
   */
  def get(key: String): Option[BsonValue] = Option(underlying.get(key))

  /**
   *  Creates a new iterator over all key/value pairs in this document
   *
   *  @return the new iterator
   */
  def iterator: Iterator[(String, BsonValue)] =
    (underlying.keySet().asScala zip underlying.values().asScala).toSeq.reverseIterator

  /**
   * Filters this document by retaining only keys satisfying a predicate.
   *  @param  p   the predicate used to test keys
   *  @return an immutable map consisting only of those key value pairs of this map where the key satisfies
   *          the predicate `p`. The resulting map wraps the original map without copying any elements.
   */
  override def filterKeys(p: String => Boolean): Document = this -- this.keys.filterNot(p)

  /**
   * Copies the BsonDocument
   * @return the copied BsonDocument
   */
  private[collection] def copyBsonDocument(): BsonDocument = {
    val bsonDocument = new BsonDocument()
    for (entry <- underlying.entrySet().asScala) bsonDocument.put(entry.getKey, entry.getValue)
    bsonDocument
  }

}

/**
 * An immutable Document helper
 */
object Document {

  def empty: immutable.Document = immutable.Document()

  /**
   * Create a new documents from a BsonDocument
   *  @param underlying a bsonDocument
   *  @return        a new Document consisting key/value pairs given by `elems`.
   */
  def apply(underlying: BsonDocument): immutable.Document = immutable.Document(underlying)

  /**
   * Create a new documents from the elems
   *  @param elems   the key/value pairs that make up the Document
   *  @return        a new Document consisting key/value pairs given by `elems`.
   */
  def apply[B](elems: (String, B)*)(implicit ev: B => BsonValue): immutable.Document = immutable.Document(elems)

  /**
   * Create a new document from the Map
   *  @param iterable   the key/value pairs that make up the Document
   *  @return        a new Document consisting key/value pairs given by `elems`.
   */
  def apply[B](iterable: Iterable[(String, B)])(implicit ev: B => BsonValue): immutable.Document =
    immutable.Document(iterable)

}

