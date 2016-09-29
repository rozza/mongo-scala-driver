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

package org.mongodb.scala.bson.codecs

import java.nio.ByteBuffer

import org.bson._
import org.bson.codecs.{Codec, DecoderContext, EncoderContext}
import org.bson.io.{BasicOutputBuffer, ByteBufferBsonInput, OutputBuffer}
import org.bson.types.ObjectId

import org.scalatest.{FlatSpec, Matchers}

class MacrosMapSpec extends FlatSpec with Matchers {

  case class Empty()
  case class Person(firstName: String, lastName: String)
  case class DefaultValue(_id: ObjectId = new ObjectId(), name: String)
  case class JavaListOfStrings(name: String, value: java.util.List[String])
  case class SeqOfStrings(name: String, value: Seq[String])
  case class RecursiveSeq(name: String, value: Seq[RecursiveSeq])

  case class MapOfStrings(name: String, value: Map[String, String])
  case class SeqOfMapOfStrings(name: String, value: Seq[Map[String, String]])
  case class RecursiveMapOfStrings(name: String, value: Seq[Map[String, RecursiveMapOfStrings]])

  case class ContainsCaseClass(name: String, friend: Person)
  case class ContainsSeqCaseClass(name: String, contacts: Seq[Person])
  case class ContainsNestedSeqCaseClass(name: String, contacts: Seq[Seq[Person]])
  case class ContainsMapOfCaseClasses(name: String, friends: Map[String, Person])
  case class ContainsMapOfMapOfCaseClasses(name: String, friends: Map[String, Map[String, Person]])

  case class OptionalValue(name: String, value: Option[String])
  case class OptionalCaseClass(name: String, value: Option[Person])
  case class OptionalRecursive(name: String, value: Option[OptionalRecursive])

  case class PlainTuple(value: (String, String))
  case class TupleWithIterable(value: (String, Seq[Int]))
  case class NestedTuple(value: (String, PlainTuple))
  case class NestedTupleWithIterable(value: (String, Seq[PlainTuple]))

  sealed class Tree
  case class Branch(b1: Tree, b2: Tree, value: Int) extends Tree
  case class Leaf(value: Int) extends Tree

  case class ContainsADT(name: String, tree: Tree)
  case class ContainsSeqADT(name: String, trees: Seq[Tree])
  case class ContainsNestedSeqADT(name: String, trees: Seq[Seq[Tree]])

  "Macros.createMapCodec" should "be able to round trip simple case classes" in {
    roundTrip(Empty(), Macros.createMapCodec[Empty]())
    roundTrip(Person("Bob", "Jones"), Macros.createMapCodec[Person]())
    roundTrip(DefaultValue(name = "Bob"), Macros.createMapCodec[DefaultValue]())
    roundTrip(SeqOfStrings("Bob", Seq("scala", "jvm")), Macros.createMapCodec[SeqOfStrings]())
    roundTrip(RecursiveSeq("Bob", Seq(RecursiveSeq("Charlie", Seq.empty))), Macros.createMapCodec[RecursiveSeq]())
    roundTrip(MapOfStrings("Bob", Map("brother" -> "Tom Jones")), Macros.createMapCodec[MapOfStrings]())
    roundTrip(SeqOfMapOfStrings("Bob", Seq(Map("brother" -> "Tom Jones"))), Macros.createMapCodec[SeqOfMapOfStrings]())
  }

  def roundTrip[T](value: T, codec: Codec[T]): Unit = {
    decode(codec, encode(codec, value)) should equal(value)
  }

  def encode[T](codec: Codec[T], value: T): OutputBuffer = {
    val buffer = new BasicOutputBuffer()
    val writer = new BsonBinaryWriter(buffer)
    codec.encode(writer, value, EncoderContext.builder.build)
    buffer
  }

  def decode[T](codec: Codec[T], buffer: OutputBuffer): T = {
    val bf = buffer.toByteArray
    val reader = new BsonBinaryReader(new ByteBufferBsonInput(new ByteBufNIO(ByteBuffer.wrap(buffer.toByteArray))))
    codec.decode(reader, DecoderContext.builder().build())
  }

}

