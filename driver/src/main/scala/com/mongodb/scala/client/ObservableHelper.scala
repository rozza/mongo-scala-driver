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

package com.mongodb.scala.client

import java.util

import com.mongodb.Block
import com.mongodb.async.SingleResultCallback
import com.mongodb.async.client.{ MongoIterable, Observables }

private[client] object ObservableHelper {

  def observe[T](mongoIterable: MongoIterable[T]): Observable[T] = Observables.observe(mongoIterable)

  def observe[T](block: (SingleResultCallback[T]) => Unit): Observable[T] =
    Observables.observe(new Block[SingleResultCallback[T]] {
      override def apply(callback: SingleResultCallback[T]): Unit = block(callback);
    })

  def observeLong(block: (SingleResultCallback[java.lang.Long]) => Unit): Observable[Long] =
    Observables.observe(new Block[SingleResultCallback[java.lang.Long]] {
      override def apply(callback: SingleResultCallback[java.lang.Long]): Unit = block(callback);
    }).asInstanceOf[Observable[Long]]

  def observeCompleted(block: (SingleResultCallback[Void]) => Unit): Observable[Completed] = {
    Observables.observe(
      new Block[SingleResultCallback[Completed]]() {
        def apply(callback: SingleResultCallback[Completed]): Unit = block(voidToCompleted(callback))
      }
    )
  }

  def observeAndFlatten[T](block: (SingleResultCallback[util.List[T]]) => Unit): Observable[T] =
    Observables.observeAndFlatten(new Block[SingleResultCallback[util.List[T]]] {
      override def apply(callback: SingleResultCallback[util.List[T]]): Unit = block(callback)
    })

  /**
   * Helper to trigger Success[Boolean] SingleResultCallbacks for Void operations
   *
   * @param callback the Success based callback.
   * @return the transforming callback
   */
  private def voidToCompleted(callback: SingleResultCallback[Completed]): SingleResultCallback[Void] = {
    new SingleResultCallback[Void] {
      override def onResult(t: Void, throwable: Throwable): Unit = {
        throwable match {
          case null => callback.onResult(Completed(), null)
          case _    => callback.onResult(null, throwable)
        }
      }
    }
  }
}
