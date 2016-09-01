/*
 * Copyright 2016 MongoDB, Inc.
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

package org.mongodb.scala.gridfs

import java.util.concurrent.TimeUnit

import scala.concurrent.duration.Duration

import com.mongodb.async.client.MongoIterable
import com.mongodb.async.client.gridfs.GridFSFindIterable

import org.mongodb.scala.{ Document, Observer, Subscription }
import org.scalamock.scalatest.proxy.MockFactory
import org.scalatest.{ FlatSpec, Matchers }

class GridFSFindObservableSpec extends FlatSpec with Matchers with MockFactory {
  val wrapper = mock[GridFSFindIterable]
  val gridFSFindObservable = GridFSFindObservable(wrapper)

  "GridFSFindObservable" should "have the same methods as the wrapped GridFSFindIterable" in {
    val mongoIterable: Set[String] = classOf[MongoIterable[Document]].getMethods.map(_.getName).toSet
    val wrapped = classOf[GridFSFindIterable].getMethods.map(_.getName).toSet -- mongoIterable
    val local = classOf[GridFSFindObservable].getMethods.map(_.getName).toSet

    wrapped.foreach((name: String) => {
      val cleanedName = name.stripPrefix("get")
      assert(local.contains(name) | local.contains(cleanedName.head.toLower + cleanedName.tail))
    })
  }

  it should "call the underlying methods" in {
    val batchSize = 20
    val filter = Document("{a: 1}")
    val limit = 10
    val maxTime = Duration(10, "second") //scalatyle:ignore
    val skip = 5
    val sort = Document("{_id: 1}")
    val observer = new Observer[GridFSFile]() {
      override def onError(throwable: Throwable): Unit = {}
      override def onSubscribe(subscription: Subscription): Unit = subscription.request(Long.MaxValue)
      override def onComplete(): Unit = {}
      override def onNext(doc: GridFSFile): Unit = {}
    }

    wrapper.expects('batchSize)(batchSize).once()
    wrapper.expects('filter)(filter).once()
    wrapper.expects('limit)(limit).once()
    wrapper.expects('maxTime)(maxTime.toMillis, TimeUnit.MILLISECONDS).once()
    wrapper.expects('noCursorTimeout)(true).once()
    wrapper.expects('skip)(skip).once()
    wrapper.expects('sort)(sort).once()
    wrapper.expects('batchSize)(2).once()
    wrapper.expects('batchCursor)(*).once()

    gridFSFindObservable.batchSize(batchSize)
    gridFSFindObservable.filter(filter)
    gridFSFindObservable.limit(limit)
    gridFSFindObservable.maxTime(maxTime)
    gridFSFindObservable.noCursorTimeout(true)
    gridFSFindObservable.skip(skip)
    gridFSFindObservable.sort(sort)
    gridFSFindObservable.head()
  }

}
