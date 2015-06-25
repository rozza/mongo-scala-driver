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

import scala.util.{ Failure, Success, Try }

import org.mongodb.scala._

private[scala] case class RecoverWithObservable[T, U >: T](
    observable:             Observable[T],
    pf:                     PartialFunction[Throwable, Observable[U]],
    throwOriginalException: Boolean                                   = false
) extends Observable[U] {

  // scalastyle:ignore cyclomatic.complexity method.length
  override def subscribe(observer: Observer[_ >: U]): Unit = {
    observable.subscribe(
      new Observer[U] {

        @volatile
        private var recoverySubscription: Option[Subscription] = None
        @volatile
        private var originalException: Option[Throwable] = None
        @volatile
        private var inRecovery: Boolean = false
        @volatile
        private var demand: Long = 0

        override def onSubscribe(subscription: Subscription): Unit = {
          val initialSub = new Subscription() {
            override def isUnsubscribed: Boolean = subscription.isUnsubscribed

            override def request(n: Long): Unit = {
              if (n < 1) {
                throw new IllegalArgumentException(s"Number requested cannot be negative: $n")
              }

              val localDemand: Long = addDemand(n)
              inRecovery match {
                case true  => recoverySubscription.get.request(localDemand)
                case false => subscription.request(localDemand)
              }
            }

            override def unsubscribe(): Unit = subscription.unsubscribe()
          }
          observer.onSubscribe(initialSub)
        }

        override def onError(throwable: Throwable): Unit = {
          originalException = Some(throwable)
          Try(pf(throwable)) recover pf match {
            case Success(recoverObservable) =>
              inRecovery = true
              recoverObservable.subscribe(
                new Observer[U] {
                  override def onError(throwable: Throwable): Unit = {
                    throwOriginalException match {
                      case true  => observer.onError(originalException.get)
                      case false => observer.onError(throwable)
                    }
                  }

                  override def onSubscribe(subscription: Subscription): Unit = {
                    recoverySubscription = Some(subscription)
                    if (demand > 0) subscription.request(demand)
                  }

                  override def onComplete(): Unit = observer.onComplete()

                  override def onNext(tResult: U): Unit = processNext(tResult)
                }
              )
            case Failure(ex) => observer.onError(throwable)
          }
        }

        override def onComplete(): Unit = observer.onComplete()

        override def onNext(tResult: U): Unit = processNext(tResult)

        /**
         * Decrement the demand counter and pass the value to the users observer
         * @param tResult the result to pass to the users observer
         */
        private def processNext(tResult: U): Unit = {
          demand -= 1
          observer.onNext(tResult)
        }

        /**
         * Adds extra demand and protects against Longs rolling over
         *
         * @param extraDemand the amount of extra demand
         * @return the updated demand
         */
        private def addDemand(extraDemand: Long): Long = {
          demand += extraDemand
          if (demand < 0) demand = Long.MaxValue
          demand
        }

      }
    )
  }
}

