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

import scala.collection.JavaConverters._

import org.bson._
import org.bson.codecs.configuration.CodecRegistries
import org.bson.codecs.{ Codec, DecoderContext, EncoderContext }
import org.bson.io.{ BasicOutputBuffer, ByteBufferBsonInput, OutputBuffer }
import org.bson.types.ObjectId

import org.scalatest.{ FlatSpec, Matchers }

class MacrosSpec extends FlatSpec with Matchers {

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

  "Macros.createCodec" should "be able to round trip simple case classes" in {
    roundTrip(Empty(), Macros.createCodec[Empty]())
    roundTrip(Person("Bob", "Jones"), Macros.createCodec[Person]())
    roundTrip(DefaultValue(name = "Bob"), Macros.createCodec[DefaultValue]())
    roundTrip(JavaListOfStrings("Bob", Seq("scala", "jvm").asJava), Macros.createCodec[JavaListOfStrings]())
    roundTrip(SeqOfStrings("Bob", Seq("scala", "jvm")), Macros.createCodec[SeqOfStrings]())
    roundTrip(RecursiveSeq("Bob", Seq(RecursiveSeq("Charlie", Seq.empty))), Macros.createCodec[RecursiveSeq]())
    roundTrip(MapOfStrings("Bob", Map("brother" -> "Tom Jones")), Macros.createCodec[MapOfStrings]())
    roundTrip(SeqOfMapOfStrings("Bob", Seq(Map("brother" -> "Tom Jones"))), Macros.createCodec[SeqOfMapOfStrings]())
  }

  it should "be able to round trip nested case classes" in {
    roundTrip(ContainsCaseClass("Bob", Person("Charlie", "Jones")), Macros.createCodec[ContainsCaseClass]())
    roundTrip(ContainsSeqCaseClass("Bob", Seq(Person("Charlie", "Jones"))), Macros.createCodec[ContainsSeqCaseClass]())
    roundTrip(ContainsNestedSeqCaseClass("Bob", Seq(Seq(Person("Charlie", "Jones")))), Macros.createCodec[ContainsNestedSeqCaseClass]())
  }

  it should "be able to round trip nested case classes in maps with help" in {
    val registry = CodecRegistries.fromRegistries(CodecRegistries.fromCodecs(Macros.createCodec[Person]()), DEFAULT_CODEC_REGISTRY)
    roundTrip(ContainsMapOfCaseClasses("Bob", Map("mother" -> Person("Charli", "Jones"))), Macros.createCodec[ContainsMapOfCaseClasses]())
    roundTrip(
      ContainsMapOfMapOfCaseClasses("Bob", Map("maternal" -> Map("mother" -> Person("Charli", "Jones")))),
      Macros.createCodec[ContainsMapOfMapOfCaseClasses]()
    )
  }

  it should "be able to round trip optional values" in {
    roundTrip(OptionalValue("Bob", None), Macros.createCodec[OptionalValue]())
    roundTrip(OptionalValue("Bob", Some("value")), Macros.createCodec[OptionalValue]())

    roundTrip(OptionalCaseClass("Bob", None), Macros.createCodec[OptionalCaseClass]())
    roundTrip(OptionalCaseClass("Bob", Some(Person("Charlie", "Jones"))), Macros.createCodec[OptionalCaseClass]())

    roundTrip(OptionalRecursive("Bob", None), Macros.createCodec[OptionalRecursive]())
    roundTrip(OptionalRecursive("Bob", Some(OptionalRecursive("Charlie", None))), Macros.createCodec[OptionalRecursive]())
  }

  it should "be able to round trip tuples" in {
    roundTrip(PlainTuple(("Bob", "Jones")), Macros.createCodec[PlainTuple]())
    roundTrip(TupleWithIterable(("Bob", Seq(2, 2))), Macros.createCodec[TupleWithIterable]())
    roundTrip(NestedTuple(("Bobby", PlainTuple(("Charlie", "Jones")))), Macros.createCodec[NestedTuple]())
    roundTrip(
      NestedTupleWithIterable(("Bobby", Seq(PlainTuple(("Charlie", "Jones")), PlainTuple(("Tom", "Jones"))))),
      Macros.createCodec[NestedTupleWithIterable]()
    )
  }

  it should "be able to round trip ADTs" in {
    val branch = Branch(Branch(Leaf(1), Leaf(2), 3), Branch(Leaf(4), Leaf(5), 6), 3) // scalastyle:ignore
    val leaf = Leaf(1)

    roundTrip(branch, Macros.createCodec[Tree]())
    roundTrip(leaf, Macros.createCodec[Tree]())

    roundTrip(ContainsADT("Bob", branch), Macros.createCodec[ContainsADT]())
    roundTrip(ContainsADT("Bob", leaf), Macros.createCodec[ContainsADT]())

    roundTrip(ContainsSeqADT("Bob", List(branch, leaf)), Macros.createCodec[ContainsSeqADT]())
    roundTrip(ContainsNestedSeqADT("Bob", List(List(branch, leaf))), Macros.createCodec[ContainsNestedSeqADT]())
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
    val reader = new BsonBinaryReader(new ByteBufferBsonInput(new ByteBufNIO(ByteBuffer.wrap(buffer.toByteArray))))
    codec.decode(reader, DecoderContext.builder().build())
  }

}
