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
package org.mongodb.scala.core.collection.mutable

import org.bson.{ BsonDocument, BsonValue }
import org.mongodb.scala.core.collection

import scala.collection.TraversableOnce

object Document {

  /**
   * Create a new empty Document
   * @return a new Document
   */
  def apply(): Document = empty

  /**
   * Create a new document from the elems
   *  @param elems   the key/value pairs that make up the Document
   *  @return        a new Document consisting key/value pairs given by `elems`.
   */
  def apply[B](elems: (String, B)*)(implicit ev: B => BsonValue): Document = apply(elems)

  /**
   * Create a new document from the Map
   *  @param iterable the key/value pairs that make up the Document
   *  @return         a new Document consisting key/value pairs given by `elems`.
   */
  def apply[B](iterable: Iterable[(String, B)])(implicit ev: B => BsonValue): Document = {
    val underlying = new BsonDocument()
    for ((k, v) <- iterable) underlying.put(k, v)
    new Document(underlying)
  }

  /**
   * Create an empty Document
   * @return a empty Document
   */
  def empty: Document = new Document(new BsonDocument())

}

case class Document(underlying: BsonDocument) extends collection.Document {

  self =>

  def apply(underlying: BsonDocument) = new Document(underlying)

  /**
   *  Adds a new key/value pair to this document.
   *  If the document already contains a mapping for the key, it will be overridden by the new value.
   *
   *  @param    kv the key/value pair.
   *  @return   the document itself
   */
  def +=[B](kv: (String, B))(implicit ev: B => BsonValue): Document = {
    underlying.put(kv._1, kv._2)
    this
  }

  /**
   * ${Add}s two or more elements to this document.
   *
   *  @param elem1 the first element to $add.
   *  @param elem2 the second element to $add.
   *  @param elems the remaining elements to $add.
   *  @return the $coll itself
   */
  def +=[B](elem1: (String, B), elem2: (String, B), elems: (String, B)*)(implicit ev: B => BsonValue): Document = this += elem1 += elem2 ++= elems

  /**
   * ${Add}s all elements produced by a TraversableOnce to this document.
   *
   *  @param xs   the TraversableOnce producing the elements to $add.
   *  @return  the document itself.
   */
  def ++=[B](xs: TraversableOnce[(String, B)])(implicit ev: B => BsonValue): Document = { xs foreach (this += _); this }

  /**
   * Adds a new key/value pair to this map.
   *  If the document already contains a mapping for the key, it will be overridden by the new value.
   *
   *  @param key    The key to update
   *  @param value  The new value
   */
  def update[B](key: String, value: B)(implicit ev: B => BsonValue) { this += ((key, value)) }

  /**
   *  Adds a new key/value pair to this document and optionally returns previously bound value.
   *  If the document already contains a mapping for the key, it will be overridden by the new value.
   *
   * @param key    the key to update
   * @param value  the new value
   * @return an option value containing the value associated with the key before the `put` operation was executed, or
   *         `None` if `key` was not defined in the document before.
   */
  def put[B](key: String, value: B)(implicit ev: B => BsonValue): Option[BsonValue] = {
    val r = get(key)
    update(key, value)
    r
  }

  /**
   * If given key is already in this document, returns associated value.
   *
   *  Otherwise, computes value from given expression `op`, stores with key in document and returns that value.
   *  @param  key the key to test
   *  @param  op  the computation yielding the value to associate with `key`, if `key` is previously unbound.
   *  @return     the value associated with key (either previously or as a result of executing the method).
   */
  def getOrElseUpdate[B](key: String, op: => B)(implicit ev: B => BsonValue): BsonValue = {
    get(key) match {
      case Some(v) => v
      case None    => val d = op; this(key) = d; d
    }
  }

  /**
   * Removes a key from this document.
   *  @param    key the key to be removed
   *  @return   the document itself.
   */
  def -=(key: String): Document = { underlying.remove(key); this }

  /**
   * Removes two or more elements from this document.
   *
   *  @param elem1 the first element to remove.
   *  @param elem2 the second element to remove.
   *  @param elems the remaining elements to remove.
   *  @return the document itself
   */
  def -=(elem1: String, elem2: String, elems: String*): Document = {
    this -= elem1
    this -= elem2
    this --= elems
  }

  /**
   * Removes all elements produced by an iterator from this $coll.
   *
   *  @param xs   the iterator producing the elements to remove.
   *  @return the $coll itself
   */
  def --=(xs: TraversableOnce[String]): Document = { xs foreach -=; this }

  /**
   * Removes a key from this document, returning the value associated previously with that key as an option.
   *  @param    key the key to be removed
   *  @return   an option value containing the value associated previously with `key`,
   *            or `None` if `key` was not defined in the document before.
   */
  def remove(key: String): Option[BsonValue] = {
    val r = get(key)
    this -= key
    r
  }

  /**
   * Retains only those mappings for which the predicate `p` returns `true`.
   *
   * @param p  The test predicate
   */
  def retain(p: (String, BsonValue) => Boolean): this.type = {
    for ((k, v) <- this.toList) // SI-7269 toList avoids ConcurrentModificationException
      if (!p(k, v)) this -= k
    this
  }

  /**
   * Removes all bindings from the document. After this operation has completed the document will be empty.
   */
  def clear() { keysIterator foreach -= }

  /**
   * Applies a transformation function to all values contained in this document.
   *  The transformation function produces new values from existing keys associated values.
   *
   * @param f  the transformation to apply
   * @return   the document itself.
   */
  def transform[B](f: (String, BsonValue) => B)(implicit ev: B => BsonValue): Document = {
    this.foreach(kv => update(kv._1, f(kv._1, kv._2)))
    this
  }

  def copy(): Document = Document(copyBsonDocument())

  /**
   * The empty document
   *   @return   an empty document
   */
  override def empty: Document = Document(new BsonDocument())
  override def clone(): Document = copy()

}
