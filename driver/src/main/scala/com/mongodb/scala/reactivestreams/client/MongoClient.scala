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

package com.mongodb.scala.reactivestreams.client

import java.io.Closeable

import com.mongodb.ConnectionString
import com.mongodb.async.client.MongoClientOptions
import com.mongodb.reactivestreams.client.{ MongoClient => JMongoClient, MongoClients }
import org.bson.Document
import org.reactivestreams.Publisher

import scala.reflect.ClassTag

object MongoClient {

  /**
   * Create a default MongoClient at localhost:27017
   *
   * @return MongoClient
   */
  def apply(): MongoClient = this("mongodb://localhost:27017")

  /**
   * Create a MongoClient instance from the MongoClientOptions
   *
   * @param clientOptions MongoClientOptions to use for the MongoClient
   * @return
   */
  def apply(clientOptions: MongoClientOptions): MongoClient = MongoClient(MongoClients.create(clientOptions))

  /**
   * Create a MongoClient instance from a connection string uri
   *
   * @param uri the connection string
   * @return MongoClient
   */
  def apply(uri: String): MongoClient = MongoClient(MongoClients.create(new ConnectionString(uri)))

}

/**
 * A client-side representation of a MongoDB cluster.  Instances can represent either a standalone MongoDB instance, a replica set,
 * or a sharded cluster.  Instance of this class are responsible for maintaining an up-to-date state of the cluster,
 * and possibly cache resources related to this, including background threads for monitoring, and connection pools.
 *
 * Instance of this class server as factories for [[MongoDatabase]] instances.
 *
 */
case class MongoClient(wrapped: JMongoClient) extends Closeable {

  /**
   * Gets the database with the given name.
   *
   * @param name the name of the database
   * @return the database
   */
  def getDatabase(name: String): MongoDatabase = MongoDatabase(wrapped.getDatabase(name))

  /**
   * Close the client, which will close all underlying cached resources, including, for example,
   * sockets and background monitoring threads.
   */
  def close(): Unit = wrapped.close()

  /**
   * Gets the options that this client uses to connect to server.
   *
   * **Note**: `MongoClientOptions` is immutable.
   *
   * @return the options
   */
  lazy val options: MongoClientOptions = wrapped.getOptions

  /**
   * Get a list of the database names
   *
   * [[http://docs.mongodb.org/manual/reference/commands/listDatabases List Databases]]
   * @return an iterable containing all the names of all the databases
   */
  def listDatabaseNames(): Publisher[String] = wrapped.listDatabaseNames()

  /**
   * Gets the list of databases
   *
   * @param clazz the class to cast the database documents to
   * @tparam T   the type of the class to use instead of `Document`.
   * @return the fluent list databases interface
   */
  def listDatabases[T]()(implicit e: T DefaultsTo Document, ct: ClassTag[T]): ListDatabasesPublisher[T] =
    ListDatabasesPublisher(wrapped.listDatabases(ct.runtimeClass.asInstanceOf[Class[T]]))
}
