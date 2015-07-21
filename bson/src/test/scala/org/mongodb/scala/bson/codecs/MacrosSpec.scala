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
import java.util

import scala.collection.JavaConverters._
import scala.reflect.ClassTag

import org.bson._
import org.bson.codecs.configuration.{ CodecProvider, CodecRegistries }
import org.bson.codecs.{ Codec, DecoderContext, EncoderContext }
import org.bson.io.{ BasicOutputBuffer, ByteBufferBsonInput, OutputBuffer }
import org.bson.types.ObjectId

import org.mongodb.scala.bson.codecs.Macros.createCodecProvider
import org.scalatest.{ FlatSpec, Matchers }

//scalastyle:off
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

  sealed class Tree
  case class Branch(b1: Tree, b2: Tree, value: Int) extends Tree
  case class Leaf(value: Int) extends Tree

  case class ContainsADT(name: String, tree: Tree)
  case class ContainsSeqADT(name: String, trees: Seq[Tree])
  case class ContainsNestedSeqADT(name: String, trees: Seq[Seq[Tree]])

  val emptyProvider: CodecProvider = createCodecProvider[Empty]()

  "Macros" should "be able to round trip simple case classes" in {
    roundTrip(Empty(), classOf[Empty])
    roundTrip(Person("Bob", "Jones"), classOf[Person])
    roundTrip(DefaultValue(name = "Bob"), classOf[DefaultValue])
    roundTrip(SeqOfStrings("Bob", Seq("scala", "jvm")), classOf[SeqOfStrings])
    roundTrip(RecursiveSeq("Bob", Seq(RecursiveSeq("Charlie", Seq.empty[RecursiveSeq]))), classOf[RecursiveSeq])
    roundTrip(MapOfStrings("Bob", Map("brother" -> "Tom Jones")), classOf[MapOfStrings])
    roundTrip(SeqOfMapOfStrings("Bob", Seq(Map("brother" -> "Tom Jones"))), classOf[SeqOfMapOfStrings])
  }

  it should "be able to round trip nested case classes" in {
    roundTrip(ContainsCaseClass("Bob", Person("Charlie", "Jones")), classOf[ContainsCaseClass], classOf[Person])
    roundTrip(ContainsSeqCaseClass("Bob", Seq(Person("Charlie", "Jones"))), classOf[ContainsSeqCaseClass], classOf[Person])
    roundTrip(ContainsNestedSeqCaseClass("Bob", Seq(Seq(Person("Charlie", "Jones")))), classOf[ContainsNestedSeqCaseClass], classOf[Person])
  }

  it should "be able to round trip nested case classes in maps" in {
    roundTrip(ContainsMapOfCaseClasses("Bob", Map("mother" -> Person("Charli", "Jones"))), classOf[ContainsMapOfCaseClasses], classOf[Person])
    roundTrip(
      ContainsMapOfMapOfCaseClasses("Bob", Map("maternal" -> Map("mother" -> Person("Charli", "Jones")))),
      classOf[ContainsMapOfMapOfCaseClasses], classOf[Person]
    )
  }

  it should "be able to round trip optional values" in {
    roundTrip(OptionalValue("Bob", None), classOf[OptionalValue])
    roundTrip(OptionalValue("Bob", Some("value")), classOf[OptionalValue])
    roundTrip(OptionalCaseClass("Bob", None), classOf[OptionalCaseClass])
    roundTrip(OptionalCaseClass("Bob", Some(Person("Charlie", "Jones"))), classOf[OptionalCaseClass], classOf[Person])

    roundTrip(OptionalRecursive("Bob", None), classOf[OptionalRecursive])
    roundTrip(OptionalRecursive("Bob", Some(OptionalRecursive("Charlie", None))), classOf[OptionalRecursive])
  }

  it should "support ADT sealed case classes" in {
    val branch = Branch(Branch(Leaf(1), Leaf(2), 3), Branch(Leaf(4), Leaf(5), 6), 3) // scalastyle:ignore
    val leaf = Leaf(1)

    roundTrip(leaf, classOf[Tree])
    roundTrip(branch, classOf[Tree])

    roundTrip(ContainsADT("Bob", branch), classOf[ContainsADT], classOf[Tree])
    roundTrip(ContainsADT("Bob", leaf), classOf[ContainsADT], classOf[Tree])

    roundTrip(ContainsSeqADT("Bob", List(branch, leaf)), classOf[ContainsSeqADT], classOf[Tree])
    roundTrip(ContainsNestedSeqADT("Bob", List(List(branch, leaf))), classOf[ContainsNestedSeqADT], classOf[Tree])
  }

  it should "not support tuples" in {
    "val provider = Macros.createCodecProvider(classOf[PlainTuple])" shouldNot compile
  }

  def roundTrip[T](value: T, provider: CodecProvider, providers: CodecProvider*)(implicit ct: ClassTag[T]): Unit = {
    val codecProviders: util.List[CodecProvider] = (provider +: providers).asJava
    val registry = CodecRegistries.fromRegistries(CodecRegistries.fromProviders(codecProviders), DEFAULT_CODEC_REGISTRY)
    val codec = registry.get(ct.runtimeClass).asInstanceOf[Codec[T]]
    roundTripCodec(value, codec)
  }

  def roundTripCodec[T](value: T, codec: Codec[T]): Unit = {
    val encoded = encode(codec, value)
    val decoded = decode(codec, encoded)
    assert(decoded == value)
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
