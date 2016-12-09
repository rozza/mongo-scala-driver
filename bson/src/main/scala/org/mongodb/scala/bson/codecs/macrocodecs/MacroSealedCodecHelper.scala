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

package org.mongodb.scala.bson.codecs.macrocodecs


import scala.collection.JavaConverters._
import scala.collection.mutable

import org.bson.codecs.configuration.{CodecConfigurationException, CodecRegistries, CodecRegistry}
import org.bson.{BsonReader, BsonType, BsonValue, BsonWriter}
import org.bson.codecs.{Codec, DecoderContext, Encoder, EncoderContext}

import org.mongodb.scala.bson.BsonNull

trait MacroSealedCodecHelper[T] extends Codec[T] {
  val classFieldName = "_t"
  val subClassesMap: Map[String, Class[_]]
  val subClassesMapInv: Map[Class[_], String]
  val clazzToCaseClassMap: Map[Class[_], Boolean]
  val classFieldTypeArgsMap: Map[String, Map[String, List[Class[_]]]]
  val encoderClass: Class[T]
  val codecRegistry: CodecRegistry

  def getInstance(className: String, fieldsData: Map[String, Any]): T
  def writeCaseClassData(className: String, writer: BsonWriter, value: T, encoderContext: EncoderContext, bsonNull: BsonValue): Unit

  private val registry = CodecRegistries.fromRegistries(List(codecRegistry, CodecRegistries.fromCodecs(this)).asJava)
  private val unknownTypeArgs = List[Class[BsonValue]](classOf[BsonValue])
  private val bsonNull = BsonNull()

  override def encode(writer: BsonWriter, value: T, encoderContext: EncoderContext): Unit = writeValue(writer, value, encoderContext)

  override def decode(reader: BsonReader, decoderContext: DecoderContext): T = {
    // Find the class name
    reader.mark()
    reader.readStartDocument()
    var optionalClassName: Option[String] = None
    while (optionalClassName.isEmpty && (reader.readBsonType ne BsonType.END_OF_DOCUMENT)) {
      val name = reader.readName
      if (name == classFieldName) {
        optionalClassName = Some(codecRegistry.get(classOf[String]).decode(reader, decoderContext))
      } else {
        reader.skipValue()
      }
    }
    reader.reset()

    // Validate the class name
    if (optionalClassName.isEmpty) {
      throw new CodecConfigurationException(s"Could not decode sealed case class. Missing '$classFieldName' field.")
    }

    val className = optionalClassName.get
    if (!subClassesMap.contains(className)) {
      throw new CodecConfigurationException(s"Could not decode sealed case class, unknown class $className.")
    }

    val fieldTypeArgsMap = classFieldTypeArgsMap(className)

    val map = mutable.Map[String, Any]()
    reader.readStartDocument()
    while (reader.readBsonType ne BsonType.END_OF_DOCUMENT) {
      val name = reader.readName
      val typeArgs = if (name == classFieldName) List(classOf[String]) else fieldTypeArgsMap.getOrElse(name, unknownTypeArgs)
      map += (name -> readValue(reader, decoderContext, typeArgs.head, typeArgs.tail, fieldTypeArgsMap))
    }
    reader.readEndDocument()
    getInstance(className, map.toMap)
  }

  override def getEncoderClass: Class[T] = encoderClass

  protected def writeValue[V](writer: BsonWriter, value: V, encoderContext: EncoderContext): Unit = {
    val clazz = value.getClass
    subClassesMapInv.get(clazz) match {
      case Some(className) => writeCaseClassData(className: String, writer: BsonWriter, value.asInstanceOf[T], encoderContext: EncoderContext, bsonNull)
      case None =>
        val codec = registry.get(clazz).asInstanceOf[Encoder[V]]
        encoderContext.encodeWithChildContext(codec, writer, value)
    }
  }

  protected def readValue[V](reader: BsonReader, decoderContext: DecoderContext, clazz: Class[V], typeArgs: List[Class[_]],
                           fieldTypeArgsMap: Map[String, List[Class[_]]]): V = {
    val currentType = reader.getCurrentBsonType
    currentType match {
      case BsonType.DOCUMENT => readDocument(reader, decoderContext, clazz, typeArgs, fieldTypeArgsMap)
      case BsonType.ARRAY => readArray(reader, decoderContext, clazz, typeArgs, fieldTypeArgsMap)
      case BsonType.NULL =>
        reader.readNull()
        null.asInstanceOf[V] // scalastyle:ignore
      case _ => registry.get(clazz).decode(reader, decoderContext)
    }
  }

  protected def readArray[V](reader: BsonReader, decoderContext: DecoderContext, clazz: Class[V], typeArgs: List[Class[_]],
                           fieldTypeArgsMap: Map[String, List[Class[_]]]): V = {
    reader.readStartArray()
    val list = mutable.ListBuffer[Any]()
    while (reader.readBsonType ne BsonType.END_OF_DOCUMENT) {
      list.append(readValue(reader, decoderContext, typeArgs.head, typeArgs.tail, fieldTypeArgsMap))
    }
    reader.readEndArray()
    list.toList.asInstanceOf[V]
  }

  protected def readDocument[V](reader: BsonReader, decoderContext: DecoderContext, clazz: Class[V], typeArgs: List[Class[_]],
                              fieldTypeArgsMap: Map[String, List[Class[_]]]): V = {
    val isCaseClass = clazzToCaseClassMap.getOrElse(clazz, false)
    if (isCaseClass) {
      registry.get(clazz).decode(reader, decoderContext)
    } else {
      val map = mutable.Map[String, Any]()
      val currentName = reader.getCurrentName
      reader.readStartDocument()
      while (reader.readBsonType ne BsonType.END_OF_DOCUMENT) {
        val name = reader.readName
        val fieldClazzTypeArgs = fieldTypeArgsMap.getOrElse(name, typeArgs)
        map += (name -> readValue(reader, decoderContext, fieldClazzTypeArgs.head, fieldClazzTypeArgs.tail, fieldTypeArgsMap))
      }
      reader.readEndDocument()
      map.toMap.asInstanceOf[V]
    }
  }
}
