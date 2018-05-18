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

package org.mongodb.scala.internal

import scala.collection.mutable

import org.mongodb.scala.{Observer, Subscription}

object TestObserver {

  def apply[A](): TestObserver[A] = {
    new TestObserver[A](new Observer[A] {
      override def onError(throwable: Throwable): Unit = {}

      override def onSubscribe(subscription: Subscription): Unit = {}

      override def onComplete(): Unit = {}

      override def onNext(tResult: A): Unit = {}
    })
  }

}

case class TestObserver[A](delegate: Observer[A]) extends Observer[A] {
  @volatile
  var subscription: Option[Subscription] = None
  var error: Option[Throwable] = None
  var completed: Boolean = false
  val results: mutable.ListBuffer[A] = mutable.ListBuffer[A]()
  private def terminated: Boolean = completed || error.isDefined

  override def onError(throwable: Throwable): Unit = {
    require(subscription.isDefined, "The Observer has not been subscribed to.")
    require(!terminated, s"onError called after the Observer has already completed or errored. $delegate")
    error = Some(throwable)
    delegate.onError(throwable)
  }

  override def onSubscribe(sub: Subscription): Unit = {
    require(subscription.isEmpty, "The Observer has already been subscribed to.")
    subscription = Some(sub)
    delegate.onSubscribe(sub)
  }

  override def onComplete(): Unit = {
    require(subscription.isDefined, "The Observer has not been subscribed to.")
    require(!terminated, s"onComplete called after the Observer has already completed or errored. $delegate")
    delegate.onComplete()
    completed = true
  }

  override def onNext(result: A): Unit = {
    require(subscription.isDefined, "The Observer has not been subscribed to.")
    require(!terminated, s"onNext called after the Observer has already completed or errored. $delegate")
    this.synchronized {
      results.append(result)
    }
    delegate.onNext(result)
  }
}
