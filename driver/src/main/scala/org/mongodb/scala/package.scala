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

package org.mongodb

import org.mongodb.scala.collection.immutable

/**
 * The Mongo Scala Driver package
 *
 * Contains type aliases and companion objects to help when using the Scala API
 *
 * @since 1.0
 */
package object scala {

  /**
   * An immutable Document implementation.
   *
   * A strictly typed `Map[String, BsonValue]` like structure that traverses the elements in insertion order. Unlike native scala maps there
   * is no variance in the value type and it always has to be a `BsonValue`.  The [[org.mongodb.scala.implicits]]
   * helper provides simple interactions with Documents taking native data types and converting them to `BsonValues`.
   *
   * @param underlying the underlying BsonDocument which stores the data.
   */
  type Document = immutable.Document
  val Document = immutable.Document

  /**
   * The result of a successful bulk write operation.
   */
  type BulkWriteResult = com.mongodb.bulk.BulkWriteResult

  /**
   * A MongoDB namespace, which includes a database name and collection name.
   */
  type MongoNamespace = com.mongodb.MongoNamespace
  /**
   * Represents preferred replica set members to which a query or command can be sent.
   */
  type ReadPreference = com.mongodb.ReadPreference
  /**
   * Represents ReadPreferences that can be combined with tags
   */
  type TaggableReadPreference = com.mongodb.TaggableReadPreference
  /**
   * A replica set tag
   */
  type Tag = com.mongodb.Tag
  /**
   * An immutable set of tags, used to select members of a replica set to use for read operations.
   */
  type TagSet = com.mongodb.TagSet
  /**
   * Controls the acknowledgment of write operations with various options.
   */
  type WriteConcern = com.mongodb.WriteConcern
  /**
   * The result of a successful write operation.  If the write was unacknowledged, then `wasAcknowledged` will return false and all
   * other methods with throw `MongoUnacknowledgedWriteException`.
   *
   * @see [[WriteConcern]]
   */
  type WriteConcernResult = com.mongodb.WriteConcernResult
  /**
   * Represents the details of a write error , e.g. a duplicate key error
   */
  type WriteError = com.mongodb.WriteError
  /**
   * Represents credentials to authenticate to a MongoDB server,as well as the source of the credentials and the authentication mechanism to
   * use.
   */
  type MongoCredential = com.mongodb.MongoCredential
  /**
   * Represents the location of a MongoDB server
   */
  type ServerAddress = com.mongodb.ServerAddress
  /**
   * An `Observable` represents a MongoDB operation.
   *
   * As such it is a provider of a potentially unbounded number of sequenced elements, publishing them according to the demand received
   * from its [[Observer]](s).
   *
   * @tparam TResult the type of element signaled by the operation.
   * @see Observables
   */
  type Observable[TResult] = com.mongodb.async.client.Observable[TResult]
  /**
   * A `Subscription` represents a one-to-one lifecycle of a [[Observer]]subscribing to an [[Observable]].
   *
   * Instances can only be used once by a single [[Observer]].
   *
   * It is used to both signal desire for data and to allow for unsubscribing.
   */
  type Subscription = com.mongodb.async.client.Subscription
  /**
   * Provides a mechanism for receiving push-based notifications.
   *
   * Will receive a call to `Observer#onSubscribe(Subscription)` on subscription to the [[Observable]].
   * No further notifications will be received until `Subscription#request(long)` is called.
   *
   * After signaling demand:
   *
   * - One or more invocations of `Observable#onNext(Object)` up to the maximum number defined by `Subscription#request(long)`
   * - Single invocation of `Observable#onError(Throwable)` or `Observer#onComplete()` which signals a terminal state after which no
   * further events will be sent
   *
   * Demand can be signaled via `Subscription#request(long)` whenever the [[Observer]] instance is capable of handling more.
   *
   * @tparam TResult The type of element signaled.
   */
  type Observer[TResult] = com.mongodb.async.client.Observer[TResult]
  /**
   * Various settings to control the behavior of a `MongoClient`.
   */
  type MongoClientSettings = com.mongodb.async.client.MongoClientSettings

}
