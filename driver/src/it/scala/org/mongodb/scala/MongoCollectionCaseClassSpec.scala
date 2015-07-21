/*
 * Copyright 2016 MongoDB, Inc.
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

import java.util.Date

import scala.collection.JavaConverters._
import org.mongodb.scala.bson.codecs.Macros._
import org.bson.codecs.configuration.CodecRegistries._

class MongoCollectionCaseClassSpec  extends RequiresMongoDBISpec {

  case class Contacts(phone: String)
  case class User(username: String, age: Int, DateOfBirth: Date, hobbies: java.util.List[String], contacts: Contacts, contact2: Contacts)

  "The Scala driver" should "handle case classes" in withDatabase(databaseName) {
    database =>
      val codecRegistry = fromRegistries(fromProviders(classOf[User], classOf[Contacts]), MongoClient.DEFAULT_CODEC_REGISTRY)
      val users = database.withCodecRegistry(codecRegistry).getCollection[User]("users")

      users.insertOne(
        User(
          age = 30,
          username = "Ross",
          DateOfBirth = new Date(),
          hobbies = List[String]("hiking", "music").asJava,
          contacts = Contacts("123 12314"),
          contact2 = Contacts("234 234234")
        )
      ).futureValue.head

      println(users.find[User]().first().head.futureValue)

  }

}
