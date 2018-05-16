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

import java.util.concurrent.CountDownLatch

import org.mongodb.scala.{Observable, Observer, Subscription}

import scala.concurrent.ExecutionContext

private[scala] case class ExecutionContextObservable[T](context: ExecutionContext, original: Observable[T]) extends Observable[T] {

  override def subscribe(observer: Observer[_ >: T]): Unit = {
    original.subscribe(SubscriptionCheckingObserver(
      new Observer[T] {
        override def onSubscribe(subscription: Subscription): Unit = withBlockingContext(() => observer.onSubscribe(subscription))

        override def onNext(tResult: T): Unit = withBlockingContext(() => observer.onNext(tResult))

        override def onError(throwable: Throwable): Unit = withContext(() => observer.onError(throwable))

        override def onComplete(): Unit = withContext(() => observer.onComplete())

        private def withContext(f: () => Unit): Unit = {
          context.execute(new Runnable {
            override def run(): Unit = f()
          })
        }

        private def withBlockingContext(f: () => Unit): Unit = {
          val latch = new CountDownLatch(1)
          withContext(() => {
            f()
            latch.countDown()
          })
          latch.await()
        }
      }
    ))
  }
}
