/*
 * Copyright 2017 MongoDB, Inc.
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

package org.mongodb.scala

import java.util.concurrent.TimeUnit

import com.mongodb.async.client.{ChangeStreamIterable, MongoIterable}
import org.mongodb.scala.internal.ObservableHelper._
import org.mongodb.scala.model.Collation
import org.mongodb.scala.model.changestream.{ChangeStreamDocument, FullDocument}

import scala.concurrent.duration.Duration

/**
 * Observable for change streams.
 *
 * '''Note:''' The [[org.mongodb.scala.model.changestream.ChangeStreamDocument]] class will not be applicable for all change stream outputs.
 * If using custom pipelines that radically change the result, the the [[ChangeStreamObservable#withDocumentClass]] method should be used
 * to provide an alternative document format.
 *
 * @param wrapped the underlying java ChangeStreamIterable
 * @tparam TResult The type of the result.
 * @since 2.2
 * @note Requires MongoDB 3.6 or greater
 */
case class ChangeStreamObservable[TResult](private val wrapped: ChangeStreamIterable[TResult]) extends Observable[ChangeStreamDocument[TResult]] {

  /**
   * Sets the fullDocument value.
   *
   * @param fullDocument the fullDocument
   * @return this
   */
  def fullDocument(fullDocument: FullDocument): ChangeStreamObservable[TResult] = {
    wrapped.fullDocument(fullDocument)
    this
  }

  /**
   * Sets the logical starting point for the new change stream.
   *
   * @param resumeToken the resume token
   * @return this
   */
  def resumeAfter(resumeToken: Document): ChangeStreamObservable[TResult] = {
    wrapped.resumeAfter(resumeToken.underlying)
    this
  }

  /**
   * Sets the number of documents to return per batch.
   *
   * @param batchSize the batch size
   * @return this
   */
  def batchSize(batchSize: Int): ChangeStreamObservable[TResult] = {
    wrapped.batchSize(batchSize)
    this
  }

  /**
   * Sets the maximum await execution time on the server for this operation.
   *
   * [[http://docs.mongodb.org/manual/reference/operator/meta/maxTimeMS/ Max Time]]
   * @param duration the duration
   * @return this
   */
  def maxAwaitTime(duration: Duration): ChangeStreamObservable[TResult] = {
    wrapped.maxAwaitTime(duration.toMillis, TimeUnit.MILLISECONDS)
    this
  }

  /**
   * Sets the collation options
   *
   * A null value represents the server default.
   *
   * @param collation the collation options to use
   * @return this
   */
  def collation(collation: Collation): ChangeStreamObservable[TResult] = {
    wrapped.collation(collation)
    this
  }

  /**
   * Returns an `Observable` containing the results of the change stream based on the document class provided.
   *
   * @param clazz the class to use for the raw result.
   * @tparam T the result type
   * @return an Observable
   */
  def withDocumentClass[T](clazz: Class[T]): Observable[T] = observe(wrapped.withDocumentClass(clazz))

  override def subscribe(observer: Observer[_ >: ChangeStreamDocument[TResult]]): Unit = observe(wrapped).subscribe(observer)
}
