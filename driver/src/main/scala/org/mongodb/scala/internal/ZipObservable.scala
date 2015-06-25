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

import java.util.concurrent.ConcurrentLinkedQueue

import scala.language.existentials

import org.mongodb.scala.{ Observable, Observer, Subscription }

private[scala] case class ZipObservable[T, U](
    observable1: Observable[T],
    observable2: Observable[U]
) extends Observable[(T, U)] {

  private val thisQueue: ConcurrentLinkedQueue[(Long, T)] = new ConcurrentLinkedQueue[(Long, T)]()
  private val thatQueue: ConcurrentLinkedQueue[(Long, U)] = new ConcurrentLinkedQueue[(Long, U)]()

  @volatile
  private var observable1Subscription: Option[Subscription] = None
  @volatile
  private var observable2Subscription: Option[Subscription] = None

  def subscribe(observer: Observer[_ >: (T, U)]): Unit = {
    observable1.subscribe(createSubObserver[T](thisQueue, observer, firstSub = true))
    observable2.subscribe(createSubObserver[U](thatQueue, observer, firstSub = false))
  }

  private def createSubObserver[A](queue: ConcurrentLinkedQueue[(Long, A)], observer: Observer[_ >: (T, U)], firstSub: Boolean): Observer[A] = {
    new Observer[A] {
      var counter: Long = 0
      override def onError(throwable: Throwable): Unit = observer.onError(throwable)

      override def onSubscribe(subscription: Subscription): Unit = {
        firstSub match {
          case true  => observable1Subscription = Some(subscription)
          case false => observable2Subscription = Some(subscription)
        }

        if (observable1Subscription.nonEmpty && observable2Subscription.nonEmpty) {
          observer.onSubscribe(jointSubscription)
        }
      }

      override def onComplete(): Unit = observer.onComplete()

      override def onNext(tResult: A): Unit = {
        counter += 1
        queue.add((counter, tResult))
        processNext(observer)
      }
    }
  }

  private def processNext(observer: Observer[_ >: (T, U)]): Unit = {
    (Option(thisQueue.peek), Option(thatQueue.peek)) match {
      case (Some((k1, v1)), Some((k2, v2))) if k1 == k2 => observer.onNext((thisQueue.poll()._2, thatQueue.poll()._2))
      case _ => // Do nothing counters don't match
    }
  }

  private val jointSubscription = new Subscription() {
    var subscribed = true
    override def isUnsubscribed: Boolean = !subscribed

    override def request(n: Long): Unit = {
      observable1Subscription.get.request(n)
      observable2Subscription.get.request(n)
    }

    override def unsubscribe(): Unit = {
      subscribed = false
      observable1Subscription.get.unsubscribe()
      observable2Subscription.get.unsubscribe()
    }
  }

}
