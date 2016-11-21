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

import org.mongodb.scala.bson.codecs.Macros.createCodecProvider
import org.mongodb.scala.bson.collection.immutable.Document
import org.scalatest.{ FlatSpec, Matchers }

//scalastyle:off
class MacrosSpec extends FlatSpec with Matchers {

  case class Empty()
  case class Person(firstName: String, lastName: String)
  case class DefaultValue(name: String, active: Boolean = false)
  case class SeqOfStrings(name: String, value: Seq[String])
  case class RecursiveSeq(name: String, value: Seq[RecursiveSeq])

  case class MapOfStrings(name: String, value: Map[String, String])
  case class SeqOfMapOfStrings(name: String, value: Seq[Map[String, String]])
  case class RecursiveMapOfStrings(name: String, value: Seq[Map[String, RecursiveMapOfStrings]])

  case class ContainsCaseClass(name: String, friend: Person)
  case class ContainsSeqCaseClass(name: String, friends: Seq[Person])
  case class ContainsNestedSeqCaseClass(name: String, friends: Seq[Seq[Person]])
  case class ContainsMapOfCaseClasses(name: String, friends: Map[String, Person])
  case class ContainsMapOfMapOfCaseClasses(name: String, friends: Map[String, Map[String, Person]])

  case class OptionalValue(name: String, value: Option[String])
  case class OptionalCaseClass(name: String, value: Option[Person])
  case class OptionalRecursive(name: String, value: Option[OptionalRecursive])

  sealed class Tree
  case class Branch(b1: Tree, b2: Tree, value: Int) extends Tree
  case class Leaf(value: Int) extends Tree

  case class ContainsADT(name: String, tree: Tree)
  case class ContainsSeqADT(name: String, trees: Seq[Tree])
  case class ContainsNestedSeqADT(name: String, trees: Seq[Seq[Tree]])

  sealed class Graph
  case class Node(name: String, value: Option[Graph]) extends Graph

  sealed class NotImplemented
  case class UnsupportedTuple(value: (String, String))
  case class UnsupportedMap(value: Map[Int, Int])

  "Macros" should "be able to round trip simple case classes" in {
    roundTrip(Empty(), "{}", classOf[Empty])
    roundTrip(Person("Bob", "Jones"), """{firstName: "Bob", lastName: "Jones"}""", classOf[Person])
    roundTrip(DefaultValue(name = "Bob"), """{name: "Bob", active: false}""", classOf[DefaultValue])
    roundTrip(SeqOfStrings("Bob", Seq("scala", "jvm")), """{name: "Bob", value: ["scala", "jvm"]}""", classOf[SeqOfStrings])
    roundTrip(RecursiveSeq("Bob", Seq(RecursiveSeq("Charlie", Seq.empty[RecursiveSeq]))), """{name: "Bob", value: [{name: "Charlie", value: []}]}""", classOf[RecursiveSeq])
    roundTrip(MapOfStrings("Bob", Map("brother" -> "Tom Jones")), """{name: "Bob", value: {brother: "Tom Jones"}}""", classOf[MapOfStrings])
    roundTrip(SeqOfMapOfStrings("Bob", Seq(Map("brother" -> "Tom Jones"))), """{name: "Bob", value: [{brother: "Tom Jones"}]}""", classOf[SeqOfMapOfStrings])
  }

  it should "be able to round trip nested case classes" in {
    roundTrip(ContainsCaseClass("Charlie", Person("Bob", "Jones")), """{name: "Charlie", friend: {firstName: "Bob", lastName: "Jones"}}""", classOf[ContainsCaseClass], classOf[Person])
    roundTrip(ContainsSeqCaseClass("Charlie", Seq(Person("Bob", "Jones"))), """{name: "Charlie", friends: [{firstName: "Bob", lastName: "Jones"}]}""", classOf[ContainsSeqCaseClass], classOf[Person])
    roundTrip(
      ContainsNestedSeqCaseClass("Charlie", Seq(Seq(Person("Bob", "Jones")), Seq(Person("Tom", "Jones")))),
      """{name: "Charlie", friends: [[{firstName: "Bob", lastName: "Jones"}], [{firstName: "Tom", lastName: "Jones"}]]}""", classOf[ContainsNestedSeqCaseClass], classOf[Person]
    )
  }

  it should "be able to round trip nested case classes in maps" in {
    roundTrip(
      ContainsMapOfCaseClasses("Bob", Map("mother" -> Person("Jane", "Jones"))),
      """{name: "Bob", friends: {mother: {firstName: "Jane", lastName: "Jones"}}}""", classOf[ContainsMapOfCaseClasses], classOf[Person]
    )
    roundTrip(
      ContainsMapOfMapOfCaseClasses("Bob", Map("maternal" -> Map("mother" -> Person("Jane", "Jones")))),
      """{name: "Bob", friends: {maternal: {mother: {firstName: "Jane", lastName: "Jones"}}}}""",
      classOf[ContainsMapOfMapOfCaseClasses], classOf[Person]
    )
  }

  it should "be able to round trip optional values" in {
    roundTrip(OptionalValue("Bob", None), """{name: "Bob", value: null}""", classOf[OptionalValue])
    roundTrip(OptionalValue("Bob", Some("value")), """{name: "Bob", value: "value"}""", classOf[OptionalValue])
    roundTrip(OptionalCaseClass("Bob", None), """{name: "Bob", value: null}""", classOf[OptionalCaseClass])
    roundTrip(
      OptionalCaseClass("Bob", Some(Person("Charlie", "Jones"))),
      """{name: "Bob", value: {firstName: "Charlie", lastName: "Jones"}}""", classOf[OptionalCaseClass], classOf[Person]
    )

    roundTrip(OptionalRecursive("Bob", None), """{name: "Bob", value: null}""", classOf[OptionalRecursive])
    roundTrip(
      OptionalRecursive("Bob", Some(OptionalRecursive("Charlie", None))),
      """{name: "Bob", value: {name: "Charlie", value: null}}""", classOf[OptionalRecursive]
    )
  }

  it should "support ADT sealed case classes" in {
    val leaf = Leaf(1)
    val branch = Branch(Branch(Leaf(1), Leaf(2), 3), Branch(Leaf(4), Leaf(5), 6), 3) // scalastyle:ignore

    def createJson(tree: Tree): String = {
      tree match {
        case l: Leaf => s"""{_t: "MacrosSpec.this.Leaf", value: ${l.value}}"""
        case b: Branch => s"""{_t: "MacrosSpec.this.Branch", b1: ${createJson(b.b1)}, b2: ${createJson(b.b2)}, value: ${b.value}}"""
        case _ => "{}"
      }
    }
    val leafJson = createJson(leaf)
    val branchJson = createJson(branch)

    roundTrip(leaf, leafJson, classOf[Tree])
    roundTrip(branch, branchJson, classOf[Tree])

    roundTrip(ContainsADT("Bob", leaf), s"""{name: "Bob", tree: $leafJson}""", classOf[ContainsADT], classOf[Tree])
    roundTrip(ContainsADT("Bob", branch), s"""{name: "Bob", tree: $branchJson}""", classOf[ContainsADT], classOf[Tree])

    roundTrip(ContainsSeqADT("Bob", List(leaf, branch)), s"""{name: "Bob", trees: [$leafJson, $branchJson]}""", classOf[ContainsSeqADT], classOf[Tree])
    roundTrip(ContainsNestedSeqADT("Bob", List(List(leaf), List(branch))), s"""{name: "Bob", trees: [[$leafJson], [$branchJson]]}""",
      classOf[ContainsNestedSeqADT], classOf[Tree])
  }

  it should "support optional values in ADT sealed classes" in {
    val nodeA = Node("nodeA", None)
    val nodeB = Node("nodeB", Some(nodeA))

    val nodeAJson = """{_t: "MacrosSpec.this.Node", name: "nodeA", value: null}"""
    val nodeBJson = s"""{_t: "MacrosSpec.this.Node", name: "nodeB", value: $nodeAJson}"""

    roundTrip(nodeA, nodeAJson, classOf[Graph])
    roundTrip(nodeB, nodeBJson, classOf[Graph])
  }

  it should "not compile case classes with unsupported values" in {
    "Macros.createCodecProvider(classOf[UnsupportedTuple])" shouldNot compile
    "Macros.createCodecProvider(classOf[UnsupportedMap])" shouldNot compile
  }

  it should "not compile if there are no concrete implementations of a sealed class" in {
    "Macros.createCodecProvider(classOf[NotImplemented])" shouldNot compile
  }

  def roundTrip[T](value: T, expected: String, provider: CodecProvider, providers: CodecProvider*)(implicit ct: ClassTag[T]): Unit = {
    val codecProviders: util.List[CodecProvider] = (provider +: providers).asJava
    val registry = CodecRegistries.fromRegistries(CodecRegistries.fromProviders(codecProviders), DEFAULT_CODEC_REGISTRY)
    val codec = registry.get(ct.runtimeClass).asInstanceOf[Codec[T]]
    roundTripCodec(value, Document(expected), codec)
  }

  def roundTripCodec[T](value: T, expected: Document, codec: Codec[T]): Unit = {
    val encoded = encode(codec, value)
    val actual = decode(documentCodec, encoded)
    assert(expected == actual, s"Encoded document: (${actual.toJson()}) did not equal: (${expected.toJson()})")

    val roundTripped = decode(codec, encode(codec, value))
    assert(roundTripped == value, s"Round Tripped case class: ($roundTripped) did not equal the original: ($value)")
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

  val documentCodec = DEFAULT_CODEC_REGISTRY.get(classOf[Document])

}
