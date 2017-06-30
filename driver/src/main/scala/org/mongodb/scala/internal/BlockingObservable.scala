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

import scala.concurrent.ExecutionContext

import org.mongodb.scala.{Observable, Observer}

private[scala] case class BlockingObservable[T](delegate: Observable[T]) extends Observable[T] {
  private val ctx = ExecutionContext.global

  override def subscribe(observer: Observer[_ >: T]): Unit = delegate.observeOn(ctx).subscribe(BlockingObserver(observer))

  private case class BlockingObserver(delegate: Observer[T]) extends Observer[T] {

    override def onNext(result: T): Unit = {
      val latch = new CountDownLatch(1)
      ctx.execute(() => {
        delegate.onNext(result)
        latch.countDown()
      })
      latch.await()
    }

    override def onError(e: Throwable): Unit = delegate.onError(e)

    override def onComplete(): Unit = delegate.onComplete()

  }

}
