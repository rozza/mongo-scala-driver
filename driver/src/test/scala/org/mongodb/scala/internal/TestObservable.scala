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

import com.mongodb.MongoException

import org.mongodb.scala.{ Observable, Observer, Subscription }

object TestObservable {

  def apply[A](from: Iterable[A]): TestObservable[A] = {
    new TestObservable(IterableObservable[A](from))
  }

  def apply[A](from: Iterable[A], failOn: Int): TestObservable[A] = {
    new TestObservable(IterableObservable[A](from), failOn)
  }

  def apply[A](from: Iterable[A], failOn: Int, errorMessage: String): TestObservable[A] = {
    new TestObservable(IterableObservable[A](from), failOn, errorMessage)
  }
}

case class TestObservable[A](
    delegate:     Observable[A] = IterableObservable[Int]((1 to 100).toStream),
    failOn:       Int           = Int.MaxValue,
    errorMessage: String        = "Failed"
) extends Observable[A] {
  var failed = false

  override def subscribe(observer: Observer[_ >: A]): Unit = {
    delegate.subscribe(
      new Observer[A] {
        var subscription: Option[Subscription] = None
        override def onError(throwable: Throwable): Unit = observer.onError(throwable)

        override def onSubscribe(sub: Subscription): Unit = {
          subscription = Some(sub)
          observer.onSubscribe(sub)
        }

        override def onComplete(): Unit = if (!failed) observer.onComplete()

        override def onNext(tResult: A): Unit = {
          if (!failed) {
            observer.onNext(tResult)
            if (tResult == failOn) {
              failed = true
              observer.onError(new MongoException(errorMessage))
            }
          }
        }
      }
    )
  }
}
