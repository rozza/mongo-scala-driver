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

import scala.collection.JavaConverters._

import org.bson.BsonDocument
import org.bson.codecs.BsonValueCodecProvider
import org.bson.codecs.configuration.CodecRegistries.fromProviders
import com.mongodb.async.client.{ListCollectionsIterable, MongoDatabase => JMongoDatabase}

import org.mongodb.scala.bson.conversions.Bson
import org.mongodb.scala.model._
import org.scalamock.scalatest.proxy.MockFactory
import org.scalatest.{FlatSpec, Matchers}

class MongoDatabaseSpec extends FlatSpec with Matchers with MockFactory {

  val wrapped = mock[JMongoDatabase]
  val clientSession = mock[ClientSession]
  val mongoDatabase = MongoDatabase(wrapped)
  val command = Document()
  val readPreference = ReadPreference.secondary()
  def observer[T] = new Observer[T]() {
    override def onError(throwable: Throwable): Unit = {}
    override def onSubscribe(subscription: Subscription): Unit = subscription.request(Long.MaxValue)
    override def onComplete(): Unit = {}
    override def onNext(doc: T): Unit = {}
  }

  "MongoDatabase" should "have the same methods as the wrapped MongoDatabase" in {
    val wrapped = classOf[JMongoDatabase].getMethods.map(_.getName)

    val local = classOf[MongoDatabase].getMethods.map(_.getName)

    wrapped.foreach((name: String) => {
      val cleanedName = name.stripPrefix("get")
      assert(local.contains(name) | local.contains(cleanedName.head.toLower + cleanedName.tail))
    })
  }

  it should "return the underlying getCollection[T]" in {
    wrapped.expects('getCollection)("collectionName", classOf[Document]).once()
    wrapped.expects('getCollection)("collectionName", classOf[BsonDocument]).once()

    mongoDatabase.getCollection("collectionName")
    mongoDatabase.getCollection[BsonDocument]("collectionName")
  }

  it should "return the underlying getName" in {
    wrapped.expects('getName)().once()

    mongoDatabase.name
  }

  it should "return the underlying getCodecRegistry" in {
    wrapped.expects('getCodecRegistry)().once()

    mongoDatabase.codecRegistry
  }

  it should "return the underlying getReadPreference" in {
    wrapped.expects('getReadPreference)().once()

    mongoDatabase.readPreference
  }

  it should "return the underlying getWriteConcern" in {
    wrapped.expects('getWriteConcern)().once()

    mongoDatabase.writeConcern
  }

  it should "return the underlying getReadConcern" in {
    wrapped.expects('getReadConcern)().once()

    mongoDatabase.readConcern
  }

  it should "return the underlying withCodecRegistry" in {
    val codecRegistry = fromProviders(new BsonValueCodecProvider())

    wrapped.expects('withCodecRegistry)(codecRegistry).once()

    mongoDatabase.withCodecRegistry(codecRegistry)
  }

  it should "return the underlying withReadPreference" in {
    wrapped.expects('withReadPreference)(readPreference).once()

    mongoDatabase.withReadPreference(readPreference)
  }

  it should "return the underlying withWriteConcern" in {
    val writeConcern = WriteConcern.MAJORITY
    wrapped.expects('withWriteConcern)(writeConcern).once()

    mongoDatabase.withWriteConcern(writeConcern)
  }

  it should "return the underlying withReadConcern" in {
    val readConcern = ReadConcern.MAJORITY
    wrapped.expects('withReadConcern)(readConcern).once()

    mongoDatabase.withReadConcern(readConcern)
  }

  it should "call the underlying runCommand[T] when writing" in {
    wrapped.expects('runCommand)(command, classOf[Document], *).once()
    wrapped.expects('runCommand)(command, classOf[BsonDocument], *).once()
    wrapped.expects('runCommand)(clientSession, command, classOf[Document], *).once()
    wrapped.expects('runCommand)(clientSession, command, classOf[BsonDocument], *).once()

    mongoDatabase.runCommand(command).subscribe(observer[Document])
    mongoDatabase.runCommand[BsonDocument](command).subscribe(observer[BsonDocument])
    mongoDatabase.runCommand(clientSession, command).subscribe(observer[Document])
    mongoDatabase.runCommand[BsonDocument](clientSession, command).subscribe(observer[BsonDocument])
  }

  it should "call the underlying runCommand[T] when reading" in {
    wrapped.expects('runCommand)(command, readPreference, classOf[Document], *).once()
    wrapped.expects('runCommand)(command, readPreference, classOf[BsonDocument], *).once()
    wrapped.expects('runCommand)(clientSession, command, readPreference, classOf[Document], *).once()
    wrapped.expects('runCommand)(clientSession, command, readPreference, classOf[BsonDocument], *).once()

    mongoDatabase.runCommand(command, readPreference).subscribe(observer[Document])
    mongoDatabase.runCommand[BsonDocument](command, readPreference).subscribe(observer[BsonDocument])
    mongoDatabase.runCommand(clientSession, command, readPreference).subscribe(observer[Document])
    mongoDatabase.runCommand[BsonDocument](clientSession, command, readPreference).subscribe(observer[BsonDocument])
  }

  it should "call the underlying drop()" in {
    wrapped.expects('drop)(*).once()
    wrapped.expects('drop)(clientSession, *).once()

    mongoDatabase.drop().subscribe(observer[Completed])
    mongoDatabase.drop(clientSession).subscribe(observer[Completed])
  }

  it should "call the underlying listCollectionNames()" in {
    wrapped.expects('listCollectionNames)().once()
    wrapped.expects('listCollectionNames)(clientSession).once()

    mongoDatabase.listCollectionNames()
    mongoDatabase.listCollectionNames(clientSession)
  }

  it should "call the underlying listCollections()" in {
    wrapped.expects('listCollections)(*).returns(stub[ListCollectionsIterable[Document]]).once()
    wrapped.expects('listCollections)(classOf[BsonDocument]).returns(stub[ListCollectionsIterable[BsonDocument]]).once()
    wrapped.expects('listCollections)(clientSession, *).returns(stub[ListCollectionsIterable[Document]]).once()
    wrapped.expects('listCollections)(clientSession, classOf[BsonDocument]).returns(stub[ListCollectionsIterable[BsonDocument]]).once()

    mongoDatabase.listCollections().subscribe(observer[Document])
    mongoDatabase.listCollections[BsonDocument]().subscribe(observer[BsonDocument])
    mongoDatabase.listCollections(clientSession).subscribe(observer[Document])
    mongoDatabase.listCollections[BsonDocument](clientSession).subscribe(observer[BsonDocument])
  }

  it should "call the underlying createCollection()" in {
    val options = CreateCollectionOptions().capped(true).validationOptions(
      ValidationOptions().validator(Document("""{level: {$gte: 10}}"""))
        .validationLevel(ValidationLevel.MODERATE)
        .validationAction(ValidationAction.WARN)
    ).indexOptionDefaults(IndexOptionDefaults().storageEngine(Document("""{storageEngine: { mmapv1: {}}}""")))
      .storageEngineOptions(Document("""{ wiredTiger: {}}"""))

    wrapped.expects('createCollection)("collectionName", *).once()
    wrapped.expects('createCollection)("collectionName", options, *).once()
    wrapped.expects('createCollection)(clientSession, "collectionName", *).once()
    wrapped.expects('createCollection)(clientSession, "collectionName", options, *).once()

    mongoDatabase.createCollection("collectionName").subscribe(observer[Completed])
    mongoDatabase.createCollection("collectionName", options).subscribe(observer[Completed])
    mongoDatabase.createCollection(clientSession, "collectionName").subscribe(observer[Completed])
    mongoDatabase.createCollection(clientSession, "collectionName", options).subscribe(observer[Completed])
  }

  it should "call the underlying createView()" in {
    val options = CreateViewOptions().collation(Collation.builder().locale("en").build())
    val pipeline = List.empty[Bson]

    wrapped.expects('createView)("viewName", "collectionName", pipeline.asJava, *).once()
    wrapped.expects('createView)("viewName", "collectionName", pipeline.asJava, options, *).once()
    wrapped.expects('createView)(clientSession, "viewName", "collectionName", pipeline.asJava, *).once()
    wrapped.expects('createView)(clientSession, "viewName", "collectionName", pipeline.asJava, options, *).once()

    mongoDatabase.createView("viewName", "collectionName", pipeline).subscribe(observer[Completed])
    mongoDatabase.createView("viewName", "collectionName", pipeline, options).subscribe(observer[Completed])
    mongoDatabase.createView(clientSession, "viewName", "collectionName", pipeline).subscribe(observer[Completed])
    mongoDatabase.createView(clientSession, "viewName", "collectionName", pipeline, options).subscribe(observer[Completed])
  }
}
