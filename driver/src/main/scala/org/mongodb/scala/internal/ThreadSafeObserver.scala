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
import java.util.concurrent.atomic.AtomicBoolean

import org.mongodb.scala.{Observer, Subscription}

import scala.annotation.tailrec

private[internal] case class ThreadSafeObserver[T](wrapped: Observer[T]) extends Observer[T] {

  private val queue = new ConcurrentLinkedQueue[T]()
  private val processing = new AtomicBoolean(false)
  @volatile
  private var subscription: Option[Subscription] = None
  @volatile
  private var error: Option[Throwable] = None
  @volatile
  private var onCompleteCalled = false
  @volatile
  private var completed = false

  override def onSubscribe(sub: Subscription): Unit = {
    subscription = Some(sub)
    wrapped.onSubscribe(sub)
  }

  override def onNext(result: T): Unit = {
    if (subscription.isEmpty) throw new IllegalStateException("The Observable has not been subscribed to.")
    queue.add(result)
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

  @tailrec
  private def processAction(): Unit = {
    if (subscription.isEmpty) return // scalastyle:ignore
    if (processing.getAndSet(true)) return // scalastyle:ignore

    var continue = true
    if (error.isDefined) {
      wrapped.onError(error.get)
      continue = false
    } else {
      val head = queue.poll()
      if (head != null) {
        wrapped.onNext(head)
      } else {
        if (onCompleteCalled) {
          wrapped.onComplete()
        }
        continue = queue.peek() != null
      }
    }
    processing.set(false)
    if (continue) processAction()
  }

  private def put(t: T): Unit = synchronized {}
  private def next(): T = synchronized { queue.poll() }
}
