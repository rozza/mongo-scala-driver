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

import scala.util.{ Failure, Success }

import org.mongodb.scala._
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{ FlatSpec, Matchers }

class ObservableImplementationSpec extends FlatSpec with Matchers with TableDrivenPropertyChecks {

  "Observables" should "call onCompleted once all results are consumed" in {

    forAll(happyObservables) {
      (observable: Observable[Int], observer: TestObserver[Int]) =>
        {
          testOnCompleted[Int](observable, observer)
        }
    }

    forAll(happyListObservables) {
      (observable: Observable[List[Int]], observer: TestObserver[List[Int]]) =>
        {
          testOnCompleted[List[Int]](observable, observer)
        }
    }

    def testOnCompleted[I](observable: Observable[I], observer: TestObserver[I]) {
      observable.subscribe(observer)

      val subscription = observer.subscription.get
      subscription.request(1)
      subscription.request(1000)

      subscription.isUnsubscribed should equal(false)
      observer.error should equal(None)
      observer.completed should equal(true)
    }
  }

  it should "be well behaved when and call onError if the Observable errors" in {
    forAll(failingObservables) {
      (observable: Observable[Int]) =>
        {
          var thrown = false
          observable.subscribe((res: Int) => (), (t: Throwable) => thrown = true)
          thrown should equal(true)
        }
    }

    forAll(failingListObservables) {
      (observable: Observable[List[Int]]) =>
        {
          var thrown = false
          observable.subscribe((res: List[Int]) => (), (t: Throwable) => thrown = true)
          thrown should equal(true)
        }
    }
  }

  it should "honor subscriptions and isUnsubscribed" in {
    forAll(happyObservables) {
      (observable: Observable[Int], observer: TestObserver[Int]) =>
        {
          testSubscriptions[Int](observable, observer)
        }
    }

    forAll(happyListObservables) {
      (observable: Observable[List[Int]], observer: TestObserver[List[Int]]) =>
        {
          testSubscriptions[List[Int]](observable, observer)
        }
    }

    def testSubscriptions[I](observable: Observable[I], observer: TestObserver[I]) {
      observable.subscribe(observer)

      val subscription = observer.subscription.get
      subscription.request(1)
      subscription.request(2)
      subscription.request(3)
      subscription.request(4)
      subscription.isUnsubscribed should equal(false)

      subscription.unsubscribe()
      subscription.isUnsubscribed should equal(true)

      observer.error should equal(None)
      observer.results.length should be <= 10
      observer.completed should equal(false)
    }
  }

  it should "honor subscriptions and isUnsubscribed without requesting data" in {
    forAll(happyObservables) {
      (observable: Observable[Int], observer: TestObserver[Int]) =>
        {
          testSubscriptions[Int](observable, observer)
        }
    }

    forAll(happyListObservables) {
      (observable: Observable[List[Int]], observer: TestObserver[List[Int]]) =>
        {
          testSubscriptions[List[Int]](observable, observer)
        }
    }

    def testSubscriptions[I](observable: Observable[I], observer: TestObserver[I]) {
      observable.subscribe(observer)

      val subscription = observer.subscription.get
      subscription.isUnsubscribed should equal(false)

      subscription.unsubscribe()
      subscription.isUnsubscribed should equal(true)

      observer.error should equal(None)
      observer.results shouldBe empty
      observer.completed should equal(false)

      subscription.request(1000)
      observer.results shouldBe empty
      observer.completed should equal(false)
    }
  }

  it should "propagate errors from the observer" in {
    forAll(happyObservables) {
      (observable: Observable[Int], observer: TestObserver[Int]) =>
        {
          testObserver[Int](observable, observer)
        }
    }

    forAll(happyListObservables) {
      (observable: Observable[List[Int]], observer: TestObserver[List[Int]]) =>
        {
          testObserver[List[Int]](observable, observer)
        }
    }

    def testObserver[I](observable: Observable[I], observer: TestObserver[I]): Unit = {
      val failObserver = TestObserver[I](new Observer[I] {
        override def onError(throwable: Throwable): Unit = {}

        override def onSubscribe(subscription: Subscription): Unit = {}

        override def onComplete(): Unit = {}

        override def onNext(tResult: I): Unit = throw new Throwable("Failed action")
      })
      observable.subscribe(failObserver)
      intercept[Throwable] {
        observer.subscription.get.request(10)
      }
    }
  }

  it should "return the length of the smallest Observable from ZipObservable" in {
    forAll(zippedObservables) {
      (observable: Observable[(Int, Int)]) =>
        {
          val observer = TestObserver[(Int, Int)]()
          observable.subscribe(observer)

          observer.subscription.get.request(100)

          observer.results should equal((1 to 50).map(i => (i, i)))
          observer.completed should equal(true)
        }
    }
  }

  it should "error if requested amount is less than 1" in {
    forAll(happyObservables) {
      (observable: Observable[Int], observer: TestObserver[Int]) =>
        {
          testObserver[Int](observable, observer)
        }
    }

    forAll(happyListObservables) {
      (observable: Observable[List[Int]], observer: TestObserver[List[Int]]) =>
        {
          testObserver[List[Int]](observable, observer)
        }
    }

    def testObserver[I](observable: Observable[I], observer: TestObserver[I]): Unit = {
      observable.subscribe(observer)
      intercept[IllegalArgumentException] {
        observer.subscription.get.request(0)
      }
    }
  }

  it should "handle multiple requests where request rolls over Long.MaxValue" in {
    forAll(happyObservables) {
      (observable: Observable[Int], observer: TestObserver[Int]) =>
        {
          testObserver[Int](observable, observer)
        }
    }

    forAll(happyListObservables) {
      (observable: Observable[List[Int]], observer: TestObserver[List[Int]]) =>
        {
          testObserver[List[Int]](observable, observer)
        }
    }

    def testObserver[I](observable: Observable[I], observer: TestObserver[I]): Unit = {
      observable.subscribe(observer)
      observer.subscription.get.request(Long.MaxValue - 1)
      observer.subscription.get.request(Long.MaxValue)

      observer.error should equal(None)
      observer.results should not be empty
      observer.completed should equal(true)
    }
  }

  val failOn = 30

  def failingObservables =
    Table(
      "observable",
      TestObservable[Int](failOn = failOn),
      AndThenObservable[Int, Int](TestObservable[Int](failOn = failOn), {
        case Success(r)  => 1000
        case Failure(ex) => 0
      }),
      FilterObservable[Int](TestObservable[Int](failOn = failOn), (i: Int) => i % 2 != 0),
      FlatMapObservable[Int, Int](TestObservable[Int](), (i: Int) => TestObservable[Int](failOn = failOn)),
      FoldLeftObservable(TestObservable[Int](1 to 100, failOn = failOn), 0, (v: Int, i: Int) => v + i),
      MapObservable[Int, Int](TestObservable[Int](failOn = failOn), (i: Int) => i * 100),
      RecoverObservable[Int, Int](TestObservable[Int](failOn = failOn), { case e: ArithmeticException => 999 }),
      RecoverWithObservable[Int, Int](TestObservable[Int](failOn = failOn), { case e: ArithmeticException => TestObservable[Int]() }),
      RecoverWithObservable[Int, Int](TestObservable[Int](failOn = failOn), { case e => TestObservable[Int](failOn = failOn) }),
      ZipObservable[Int, Int](TestObservable[Int](), TestObservable[Int](failOn = failOn)).map[Int](a => a._1),
      ZipObservable[Int, Int](TestObservable[Int](failOn = failOn), TestObservable[Int]()).map[Int](a => a._1)
    )

  val failingListObservables =
    Table[Observable[List[Int]]](
      "observable",
      FoldLeftObservable[Int, List[Int]](TestObservable[Int](failOn = failOn), List[Int](), (l, i) => l :+ i)
    )

  def happyObservables =
    Table(
      ("observable", "observer"),
      (TestObservable[Int](), TestObserver[Int]()),
      (AndThenObservable[Int, Int](TestObservable[Int](), {
        case Success(r)  => 1000
        case Failure(ex) => 0
      }), TestObserver[Int]()),
      (FilterObservable[Int](TestObservable[Int](), (i: Int) => i % 2 != 0), TestObserver[Int]()),
      (FlatMapObservable[Int, Int](TestObservable[Int](), (i: Int) => TestObservable[Int](1 to 1)), TestObserver[Int]()),
      (FlatMapObservable[Int, Int](TestObservable[Int](1 to 1), (i: Int) => TestObservable[Int]()), TestObserver[Int]()),
      (FoldLeftObservable(TestObservable[Int](1 to 100), 0, (v: Int, i: Int) => v + i), TestObserver[Int]()),
      (MapObservable[Int, Int](TestObservable[Int](), (i: Int) => i * 100), TestObserver[Int]()),
      (RecoverObservable[Int, Int](TestObservable[Int](), { case e: ArithmeticException => 999 }), TestObserver[Int]()),
      (RecoverWithObservable[Int, Int](TestObservable[Int](), { case t => TestObservable[Int]() }), TestObserver[Int]()),
      (RecoverWithObservable[Int, Int](TestObservable[Int](1 to 10, failOn = 1), { case t => TestObservable[Int]() }), TestObserver[Int]()),
      (IterableObservable((1 to 100).toStream), TestObserver[Int]()),
      (ZipObservable[Int, Int](TestObservable[Int](), TestObservable[Int]()).map[Int](a => a._1), TestObserver[Int]())
    )

  def happyListObservables =
    Table[Observable[List[Int]], TestObserver[List[Int]]](
      ("observable", "observer"),
      (FoldLeftObservable[Int, List[Int]](TestObservable[Int](), List[Int](), (l, i) => l :+ i), TestObserver[List[Int]]())
    )

  def zippedObservables =
    Table[Observable[(Int, Int)]](
      "observable",
      ZipObservable[Int, Int](TestObservable[Int](1 to 50), TestObservable[Int]()),
      ZipObservable[Int, Int](TestObservable[Int](), TestObservable[Int](1 to 50))
    )

}
