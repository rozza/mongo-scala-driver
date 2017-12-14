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

package org.mongodb.scala
import java.util.concurrent.TimeUnit

import scala.concurrent.duration.Duration

import com.mongodb.async.client.{DistinctIterable, MongoIterable}

import org.mongodb.scala.model.Collation
import org.scalamock.scalatest.proxy.MockFactory
import org.scalatest.{FlatSpec, Matchers}

class DistinctObservableSpec extends FlatSpec with Matchers with MockFactory {

  "DistinctObservable" should "have the same methods as the wrapped DistinctObservable" in {
    val mongoIterable: Set[String] = classOf[MongoIterable[Document]].getMethods.map(_.getName).toSet
    val wrapped = classOf[DistinctIterable[Document]].getMethods.map(_.getName).toSet -- mongoIterable
    val local = classOf[DistinctObservable[Document]].getMethods.map(_.getName).toSet

    wrapped.foreach((name: String) => {
      val cleanedName = name.stripPrefix("get")
      assert(local.contains(name) | local.contains(cleanedName.head.toLower + cleanedName.tail))
    })
  }

  it should "call the underlying methods" in {
    val wrapper = mock[DistinctIterable[Document]]
    val observable = DistinctObservable(wrapper)

    val filter = Document("a" -> 1)
    val duration = Duration(1, TimeUnit.SECONDS)
    val collation = Collation.builder().locale("en").build()
    val observer = new Observer[Document]() {
      override def onError(throwable: Throwable): Unit = {}
      override def onSubscribe(subscription: Subscription): Unit = subscription.request(Long.MaxValue)
      override def onComplete(): Unit = {}
      override def onNext(doc: Document): Unit = {}
    }

    wrapper.expects('filter)(filter).once()
    wrapper.expects('maxTime)(duration.toMillis, TimeUnit.MILLISECONDS).once()

    wrapper.expects('collation)(collation).once()
    wrapper.expects('batchSize)(Int.MaxValue).once()
    wrapper.expects('batchCursor)(*).once()

    observable.filter(filter)
    observable.maxTime(duration)
    observable.collation(collation)
    observable.subscribe(observer)
  }
}
