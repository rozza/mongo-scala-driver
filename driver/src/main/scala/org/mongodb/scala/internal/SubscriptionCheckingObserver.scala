/*
 * Copyright 2017 MongoDB, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.mongodb.scala.internal

import org.mongodb.scala.{Observer, Subscription}

object SubscriptionCheckingObserver {
  def apply[T](observer: Observer[T]): SubscriptionCheckingObserver[T] = {
    observer match {
      case subscriptionCheckingObserver: SubscriptionCheckingObserver[T] => subscriptionCheckingObserver
      case wrapped: Observer[T] => new SubscriptionCheckingObserver[T](wrapped)
    }
  }
}

private[scala] class SubscriptionCheckingObserver[T](wrapped: Observer[T]) extends Observer[T] {
  @volatile
  private var subscription: Option[Subscription] = None

  override def onSubscribe(sub: Subscription): Unit = {
    require(subscription.isEmpty, "The Observer has already been subscribed to.")
    subscription = Some(sub)
    wrapped.onSubscribe(sub)
  }

  override def onNext(result: T): Unit = wrapped.onNext(result)

  override def onError(e: Throwable): Unit = wrapped.onError(e)

  override def onComplete(): Unit = wrapped.onComplete()
}
