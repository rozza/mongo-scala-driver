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

import java.util.concurrent.ConcurrentLinkedQueue

import org.mongodb.scala.{Observable, Observer, Subscription}

import scala.concurrent.ExecutionContext

private[scala] case class ExecutionContextObservable[T](observable: Observable[T], context: ExecutionContext) extends Observable[T] {

  // scalastyle:off method.length
  override def subscribe(observer: Observer[_ >: T]): Unit = {
    observable.subscribe(SubscriptionCheckingObserver(
      new Observer[T] {
        private val queue = new ConcurrentLinkedQueue[T]()
        @volatile
        private var onSubscribeCalled = false
        @volatile
        private var error: Option[Throwable] = None
        @volatile
        private var onCompleteCalled = false

        override def onSubscribe(subscription: Subscription): Unit = withContext(() => {
          onSubscribeCalled = true
          observer.onSubscribe(subscription)
          processAction()
        })

        override def onNext(tResult: T): Unit = {
          queue.add(tResult)
          processAction()
        }

        override def onError(throwable: Throwable): Unit = {
          error = Some(throwable)
          processAction()
        }

        override def onComplete(): Unit = {
          onCompleteCalled = true
          processAction()
        }

        def processAction(): Unit = synchronized {
          if (!onSubscribeCalled) return // scalastyle:ignore
          if (error.isDefined) {
            withContext(() => observer.onError(error.get))
          } else {
            val next = queue.poll()
            if (next != null) {
              withContext(() => {
                observer.onNext(next)
                processAction()
              })
            } else if (onCompleteCalled) {
              withContext(() => observer.onComplete())
            }
          }
        }

        private def withContext(f: () => Unit): Unit = {
          context.execute(new Runnable {
            override def run(): Unit = f()
          })
        }
      }
    ))
  }
}
