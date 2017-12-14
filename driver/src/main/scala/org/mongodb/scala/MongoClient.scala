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

import java.io.Closeable

import com.mongodb.ConnectionString
import com.mongodb.async.SingleResultCallback
import com.mongodb.async.client.{MongoClients, MongoClient => JMongoClient}
import org.bson.codecs.configuration.CodecRegistry
import org.mongodb.scala.bson.DefaultHelper.DefaultsTo
import org.mongodb.scala.connection._
import org.mongodb.scala.internal.ObservableHelper.observe

import scala.reflect.ClassTag

/**
 * Companion object for creating new [[MongoClient]] instances
 *
 * @since 1.0
 */
object MongoClient {

  /**
   * Create a default MongoClient at localhost:27017
   *
   * @return MongoClient
   */
  def apply(): MongoClient = apply("mongodb://localhost:27017")

  /**
   * Create a MongoClient instance from a connection string uri
   *
   * @param uri the connection string
   * @return MongoClient
   */
  def apply(uri: String): MongoClient = MongoClient(uri, None)

  /**
   * Create a MongoClient instance from a connection string uri
   *
   * @param uri the connection string
   * @param mongoDriverInformation any driver information to associate with the MongoClient
   * @return MongoClient
   * @note the `mongoDriverInformation` is intended for driver and library authors to associate extra driver metadata with the connections.
   */
  def apply(uri: String, mongoDriverInformation: Option[MongoDriverInformation]): MongoClient = {
    val connectionString = new ConnectionString(uri)
    val builder = MongoClientSettings.builder()
      .codecRegistry(DEFAULT_CODEC_REGISTRY)
      .clusterSettings(ClusterSettings.builder().applyConnectionString(connectionString).build())
      .connectionPoolSettings(ConnectionPoolSettings.builder().applyConnectionString(connectionString).build())
      .serverSettings(ServerSettings.builder().build())
      .sslSettings(SslSettings.builder().applyConnectionString(connectionString).build())
      .socketSettings(SocketSettings.builder().applyConnectionString(connectionString).build())

    Option(connectionString.getStreamType).map(_.toLowerCase) match {
      case Some("netty") => builder.streamFactoryFactory(NettyStreamFactoryFactory())
      case Some("nio2")  => builder.streamFactoryFactory(AsynchronousSocketChannelStreamFactoryFactory())
      case _             =>
    }

    Option(connectionString.getCredential).map(credential => builder.credential(credential))
    Option(connectionString.getReadPreference).map(readPreference => builder.readPreference(readPreference))
    Option(connectionString.getReadConcern).map(readConcern => builder.readConcern(readConcern))
    Option(connectionString.getWriteConcern).map(writeConcern => builder.writeConcern(writeConcern))
    Option(connectionString.getApplicationName).map(applicationName => builder.applicationName(applicationName))
    builder.compressorList(connectionString.getCompressorList)

    apply(builder.build(), mongoDriverInformation)
  }

  /**
   * Create a MongoClient instance from the MongoClientSettings
   *
   * @param clientSettings MongoClientSettings to use for the MongoClient
   * @return MongoClient
   */
  def apply(clientSettings: MongoClientSettings): MongoClient = MongoClient(clientSettings, None)

  /**
   * Create a MongoClient instance from the MongoClientSettings
   *
   * @param clientSettings MongoClientSettings to use for the MongoClient
   * @param mongoDriverInformation any driver information to associate with the MongoClient
   * @return MongoClient
   * @note the `mongoDriverInformation` is intended for driver and library authors to associate extra driver metadata with the connections.
   */
  def apply(clientSettings: MongoClientSettings, mongoDriverInformation: Option[MongoDriverInformation]): MongoClient = {
    val builder = mongoDriverInformation match {
      case Some(info) => MongoDriverInformation.builder(info)
      case None       => MongoDriverInformation.builder()
    }
    builder.driverName(BuildInfo.name).driverVersion(BuildInfo.version).driverPlatform(s"Scala/${BuildInfo.scalaVersion}")
    MongoClient(MongoClients.create(clientSettings, builder.build()))
  }

  val DEFAULT_CODEC_REGISTRY: CodecRegistry = org.mongodb.scala.bson.codecs.DEFAULT_CODEC_REGISTRY
}

/**
 * A client-side representation of a MongoDB cluster.  Instances can represent either a standalone MongoDB instance, a replica set,
 * or a sharded cluster.  Instance of this class are responsible for maintaining an up-to-date state of the cluster,
 * and possibly cache resources related to this, including background threads for monitoring, and connection pools.
 *
 * Instance of this class server as factories for [[MongoDatabase]] instances.
 *
 * @param wrapped the underlying java MongoClient
 * @since 1.0
 */
case class MongoClient(private val wrapped: JMongoClient) extends Closeable {

  /**
   * Creates a client session.
   *
   * '''Note:''' A ClientSession instance can not be used concurrently in multiple asynchronous operations.
   *
   * @param options  the options for the client session
   * @since 2.2
   * @note Requires MongoDB 3.6 or greater
   */
  def startSession(options: ClientSessionOptions): Observable[ClientSession] =
    observe(wrapped.startSession(options, _: SingleResultCallback[ClientSession]))

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
   * Gets the settings that this client uses to connect to server.
   *
   * **Note**: `MongoClientSettings` is immutable.
   *
   * @return the settings
   */
  lazy val settings: MongoClientSettings = wrapped.getSettings

  /**
   * Get a list of the database names
   *
   * [[http://docs.mongodb.org/manual/reference/commands/listDatabases List Databases]]
   * @return an iterable containing all the names of all the databases
   */
  def listDatabaseNames(): Observable[String] = observe(wrapped.listDatabaseNames())

  /**
   * Get a list of the database names
   *
   * [[http://docs.mongodb.org/manual/reference/commands/listDatabases List Databases]]
   *
   * @param clientSession the client session with which to associate this operation
   * @return an iterable containing all the names of all the databases
   * @since 2.2
   * @note Requires MongoDB 3.6 or greater
   */
  def listDatabaseNames(clientSession: ClientSession): Observable[String] = observe(wrapped.listDatabaseNames(clientSession))

  /**
   * Gets the list of databases
   *
   * @tparam TResult   the type of the class to use instead of `Document`.
   * @return the fluent list databases interface
   */
  def listDatabases[TResult]()(implicit e: TResult DefaultsTo Document, ct: ClassTag[TResult]): ListDatabasesObservable[TResult] =
    ListDatabasesObservable(wrapped.listDatabases(ct))

  /**
   * Gets the list of databases
   *
   * @param clientSession the client session with which to associate this operation
   * @tparam TResult the type of the class to use instead of `Document`.
   * @return the fluent list databases interface
   * @since 2.2
   * @note Requires MongoDB 3.6 or greater
   */
  def listDatabases[TResult](clientSession: ClientSession)(implicit e: TResult DefaultsTo Document, ct: ClassTag[TResult]): ListDatabasesObservable[TResult] =
    ListDatabasesObservable(wrapped.listDatabases(clientSession, ct))

}
