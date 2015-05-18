package com.mongodb.scala

package object client {

  type Observable[T] = com.mongodb.async.client.Observable[T]
  type Subscription = com.mongodb.async.client.Subscription
  type Observer[T] = com.mongodb.async.client.Observer[T]

  type Document = com.mongodb.scala.core.collection.immutable.Document
  val Document = com.mongodb.scala.core.collection.immutable.Document

}
