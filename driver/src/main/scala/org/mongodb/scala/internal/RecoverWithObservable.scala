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
        private object Locker

        /* protected by Locker */
        private var recoverySubscription: Option[Subscription] = None
        private var originalException: Option[Throwable] = None
        private var inRecovery: Boolean = false
        private var demand: Long = 0
        /* protected by Locker */

        override def onSubscribe(subscription: Subscription): Unit = {
          val initialSub = new Subscription() {
            override def isUnsubscribed: Boolean = subscription.isUnsubscribed

            override def request(n: Long): Unit = {
              if (n < 1) {
                throw new IllegalArgumentException(s"Number requested cannot be negative: $n")
              }

              var localDemand: Long = 0
              var localInRecovery = false
              insideLock({
                localDemand = addDemand(n)
                localInRecovery = inRecovery
              })

              localInRecovery match {
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
            case Success(recoverObservable) => {
              insideLock { inRecovery = true }
              recoverObservable.subscribe(
                new Observer[U] {
                  override def onError(throwable: Throwable): Unit = {
                    throwOriginalException match {
                      case true  => observer.onError(originalException.get)
                      case false => observer.onError(throwable)
                    }
                  }

                  override def onSubscribe(subscription: Subscription): Unit = {
                    var localDemand: Long = 0
                    insideLock({
                      localDemand = demand
                      recoverySubscription = Some(subscription)
                    })
                    if (localDemand > 0) subscription.request(localDemand)
                  }

                  override def onComplete(): Unit = {
                    observer.onComplete()
                  }

                  override def onNext(tResult: U): Unit = {
                    insideLock { demand -= 1 }
                    observer.onNext(tResult)
                  }
                }
              )
            }
            case Failure(ex) => observer.onError(throwable)
          }
        }

        override def onComplete(): Unit = observer.onComplete()

        override def onNext(tResult: U): Unit = {
          insideLock { demand -= 1 }
          observer.onNext(tResult)
        }

        private def insideLock(f: => Unit): Unit = Locker.synchronized { f }

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

