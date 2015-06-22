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

import com.mongodb.MongoException

import org.mongodb.scala._
import org.scalatest.{ FlatSpec, Matchers }

class ScalaObservableSpec extends FlatSpec with Matchers {

  "ScalaObservable" should "allow for inline subscription" in {
    var counter = 0
    observable().subscribe((res: Int) => counter += 1)
    counter should equal(100)

    var thrown = false
    observable(fail = true).subscribe((res: Int) => (), (t: Throwable) => thrown = true)
    thrown should equal(true)

    var completed = false
    observable().subscribe((res: Int) => (), (t: Throwable) => (), () => completed = true)
    completed should equal(true)
  }

  it should "have a foreach method" in {
    var counter = 0
    observable().foreach((res: Int) => counter += 1)
    counter should equal(100)
  }

  it should "have a transform method" in {
    var completed = false
    var lastSeenString = ""
    observable[Int]()
      .transform((res: Int) => res.toString, (ex: Throwable) => ex)
      .subscribe((s: String) => lastSeenString = s, (t: Throwable) => (), () => completed = true)
    lastSeenString should equal("100")
    completed should equal(true)
  }

  it should "have a map method" in {
    def myObservable(fail: Boolean = false): Observable[String] = observable[Int](fail = fail).map((res: Int) => res.toString)

    var lastSeenString = ""
    var completed = false
    observable[Int]()
      .map((res: Int) => res.toString)
      .subscribe((s: String) => lastSeenString = s, (t: Throwable) => (), () => completed = true)
    lastSeenString should equal("100")
  }

  it should "have a flatMap method" in {
    def myObservable(fail: Boolean = false): Observable[String] =
      observable[Int](fail = fail).flatMap((res: Int) => observable(List(res.toString)))

    var lastSeenString = ""
    myObservable().subscribe((s: String) => lastSeenString = s)
    lastSeenString should equal("100")

    var errorSeen: Option[Throwable] = None
    myObservable(true).subscribe((s: String) => (), (fail: Throwable) => errorSeen = Some(fail))
    errorSeen.getOrElse(None) shouldBe a[Throwable]

    var completed = false
    myObservable().subscribe((s: String) => (), (t: Throwable) => t, () => completed = true)
    completed should equal(true)

  }

  it should "have a filter method" in {
    def myObservable(fail: Boolean = false): Observable[Int] = observable[Int](fail = fail).filter((i: Int) => i % 2 != 0)

    var lastSeen = 0
    myObservable().subscribe((i: Int) => lastSeen = i)
    lastSeen should equal(99)

    var errorSeen: Option[Throwable] = None
    myObservable(true).subscribe((s: Int) => (), (fail: Throwable) => errorSeen = Some(fail))
    errorSeen.getOrElse(None) shouldBe a[Throwable]

    var completed = false
    myObservable().subscribe((s: Int) => (), (t: Throwable) => t, () => completed = true)
    completed should equal(true)
  }

  it should "have a withFilter method" in {
    def myObservable(fail: Boolean = false): Observable[Int] = observable[Int](fail = fail).withFilter((i: Int) => i % 2 != 0)

    var lastSeen = 0
    myObservable().subscribe((i: Int) => lastSeen = i)
    lastSeen should equal(99)

    var errorSeen: Option[Throwable] = None
    myObservable(true).subscribe((s: Int) => (), (fail: Throwable) => errorSeen = Some(fail))
    errorSeen.getOrElse(None) shouldBe a[Throwable]

    var completed = false
    myObservable().subscribe((s: Int) => (), (t: Throwable) => t, () => completed = true)
    completed should equal(true)
  }

  it should "have a foldLeft method" in {
    def myObservable(fail: Boolean = false): Observable[Int] = {
      observable[Int](fail = fail).foldLeft(0)((l: Int, i) => l + 1)
    }

    var count = 0
    myObservable().subscribe((i: Int) => count = i)
    count should equal(100)

    var errorSeen: Option[Throwable] = None
    myObservable(true).subscribe((s: Int) => (), (fail: Throwable) => errorSeen = Some(fail))
    errorSeen.getOrElse(None) shouldBe a[Throwable]

    var completed = false
    myObservable().subscribe((s: Int) => (), (t: Throwable) => t, () => completed = true)
    completed should equal(true)
  }

  it should "have a recover method" in {

    var lastSeen = 0
    observable().recover({ case e: ArithmeticException => 999 }).subscribe((i: Int) => lastSeen = i)
    lastSeen should equal(100)

    var errorSeen: Option[Throwable] = None
    observable[Int](fail = true)
      .recover({ case e: ArithmeticException => 999 })
      .subscribe((s: Int) => (), (fail: Throwable) => errorSeen = Some(fail))
    errorSeen.getOrElse(None) shouldBe a[Throwable]

    lastSeen = 0
    observable(fail = true)
      .transform(i => i, (t: Throwable) => new ArithmeticException())
      .recover({ case e: ArithmeticException => 999 })
      .subscribe((i: Int) => lastSeen = i)
    lastSeen should equal(999)

  }

  it should "have a recoverWith method" in {
    var lastSeen = 0
    var completed = false
    observable()
      .recoverWith({ case e: ArithmeticException => observable(1000 to 1001) })
      .subscribe((i: Int) => lastSeen = i, (t: Throwable) => (), () => completed = true)
    lastSeen should equal(100)
    completed should equal(true)

    var errorSeen: Option[Throwable] = None
    completed = false
    observable[Int](fail = true)
      .recoverWith({ case e: ArithmeticException => observable[Int](1000 to 1001) })
      .subscribe((i: Int) => lastSeen = i, (fail: Throwable) => errorSeen = Some(fail), () => completed = true)
    errorSeen.getOrElse(None) shouldBe a[Throwable]
    lastSeen should equal(50)
    completed should equal(false)

    lastSeen = 0
    observable(fail = true)
      .transform(i => i, (t: Throwable) => new ArithmeticException())
      .recoverWith({ case e: ArithmeticException => observable(1000 to 1001) })
      .subscribe((i: Int) => lastSeen = i)
    lastSeen should equal(1001)

  }

  it should "have a zip method" in {
    var lastSeen: (Int, String) = (0, "0")
    observable[Int]().zip(observable().map(i => i.toString)).subscribe((result: (Int, String)) => lastSeen = result)
    lastSeen should equal((100, "100"))
  }

  it should "have a fallBackTo method" in {
    var lastSeen = 0
    observable().fallbackTo(observable[Int](1000 to 1001)).subscribe((i: Int) => lastSeen = i)
    lastSeen should equal(100)

    lastSeen = 0
    observable(fail = true)
      .fallbackTo(observable[Int](1000 to 1001))
      .subscribe((i: Int) => lastSeen = i)
    lastSeen should equal(1001)

    var errorMessage = ""
    TestObservable[Int](1 to 100, 10, "Original Error")
      .fallbackTo(TestObservable[Int](1000 to 1001, 1000, "Fallback Error"))
      .subscribe((i: Int) => i, (t: Throwable) => errorMessage = t.getMessage)
    errorMessage should equal("Original Error")
  }

  it should "have an andThen method" in {
    var lastSeen = 0
    def myObservable(fail: Boolean = false): Observable[Int] = {
      observable[Int](1 to 100, fail = fail) andThen {
        case Success(r)  => lastSeen = r
        case Failure(ex) => lastSeen = -999
      }
    }

    myObservable().subscribe((i: Int) => lastSeen = i)
    lastSeen should equal(100)

    lastSeen = 0
    var errorSeen: Option[Throwable] = None
    myObservable(true).subscribe((s: Int) => (), (fail: Throwable) => errorSeen = Some(fail))
    errorSeen.getOrElse(None) shouldBe a[Throwable]
    lastSeen should equal(-999)

    lastSeen = 0
    var completed = false
    myObservable().subscribe((s: Int) => (), (t: Throwable) => t, () => completed = true)
    lastSeen should equal(100)
    completed should equal(true)
  }

  it should "work with for comprehensions" in {
    def f = observable(1 to 5)
    def g = observable(100 to 100)
    val h = for {
      x: Int <- f // returns Observable(1 to 5)
      y: Int <- g // returns Observable(100 to 100)
    } yield x + y

    var results = List(0)
    var completed = false
    h.foldLeft(List[Int]())((l, i) => l :+ i).subscribe((s: List[Int]) => results = s, (t: Throwable) => t, () => completed = true)

    results should equal(101 to 105)
    completed should equal(true)

    var resultsFlatMap = List(0)
    completed = false

    val fh: Observable[Int] = f flatMap { (x: Int) => g map { (y: Int) => x + y } }
    fh.foldLeft(List[Int]())((l, i) => l :+ i).subscribe((s: List[Int]) => resultsFlatMap = s, (t: Throwable) => t, () => completed = true)
    resultsFlatMap should equal(101 to 105)
    completed should equal(true)
  }

  it should "work with andThen as expected" in {
    var lastSeen = 0
    var completed = false
    observable(1 to 10) andThen {
      case r => throw new MongoException("Exception")
    } andThen {
      case Success(_) => lastSeen = 500
      case Failure(t) => lastSeen = -100
    } subscribe ((s: Int) => lastSeen = s, (t: Throwable) => t, () => completed = true)

    lastSeen should equal(500)
    completed should equal(true)
  }

  def observable[A](from: Iterable[A] = (1 to 100).toIterable, fail: Boolean = false): Observable[A] = {
    fail match {
      case true  => TestObservable[A](Observable(from), failOn = 50)
      case false => TestObservable[A](Observable(from))
    }
  }
}
