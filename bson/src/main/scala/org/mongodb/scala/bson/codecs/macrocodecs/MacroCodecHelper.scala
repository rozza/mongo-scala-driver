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

import org.bson._
import org.bson.codecs.configuration.{CodecRegistries, CodecRegistry}
import org.bson.codecs.{Codec, DecoderContext, Encoder, EncoderContext}

trait MacroCodecHelper[T] extends Codec[T] {

  val codecRegistry: CodecRegistry
  val fieldTypeArgsMap: Map[String, List[Class[_]]]
  val encoderClazz: Class[T]
  val clazzToCaseClassMap: Map[Class[_], Boolean]
  def getInstance(fieldsData: Map[String, Any]): T
  def writeCaseClassData(writer: BsonWriter, value: T, encoderContext: EncoderContext, nullValue: BsonValue): Unit

  protected val registry: CodecRegistry =  CodecRegistries.fromRegistries(List(codecRegistry, CodecRegistries.fromCodecs(this)).asJava)
  protected val unknownTypeArgs: List[Class[BsonValue]] = List[Class[BsonValue]](classOf[BsonValue])
  protected val bsonNull: BsonValue = new BsonNull()

  override def encode(writer: BsonWriter, value: T, encoderContext: EncoderContext): Unit = {
    writeValue(writer, value, encoderContext)
  }

  override def decode(reader: BsonReader, decoderContext: DecoderContext): T = {
    val map = mutable.Map[String, Any]()
    reader.readStartDocument()
    while (reader.readBsonType ne BsonType.END_OF_DOCUMENT) {
      val name = reader.readName
      val typeArgs = fieldTypeArgsMap.getOrElse(name, unknownTypeArgs)
      map += (name -> readValue(reader, decoderContext, typeArgs.head, typeArgs.tail))
    }
    reader.readEndDocument()
    getInstance(map.toMap)
  }

  override def getEncoderClass: Class[T] = encoderClazz

  protected def writeValue[V](writer: BsonWriter, value: V, encoderContext: EncoderContext): Unit = {
    if (value.getClass.isAssignableFrom(encoderClazz)) {
      writeCaseClassData(writer: BsonWriter, value.asInstanceOf[T], encoderContext: EncoderContext, bsonNull)
    } else {
      val codec = registry.get(value.getClass).asInstanceOf[Encoder[V]]
      encoderContext.encodeWithChildContext(codec, writer, value)
    }
  }

  protected def readValue[V](reader: BsonReader, decoderContext: DecoderContext, clazz: Class[V], typeArgs: List[Class[_]]): V = {
    val currentType = reader.getCurrentBsonType
    currentType match {
      case BsonType.DOCUMENT => readDocument(reader, decoderContext, clazz, typeArgs)
      case BsonType.ARRAY => readArray(reader, decoderContext, clazz, typeArgs)
      case BsonType.NULL =>
        reader.readNull()
        null.asInstanceOf[V] // scalastyle:ignore
      case _ => registry.get(clazz).decode(reader, decoderContext)
    }
  }

  protected def readArray[V](reader: BsonReader, decoderContext: DecoderContext, clazz: Class[V], typeArgs: List[Class[_]]): V = {
    reader.readStartArray()
    val list = mutable.ListBuffer[Any]()
    while (reader.readBsonType ne BsonType.END_OF_DOCUMENT) {
      list.append(readValue(reader, decoderContext, typeArgs.head, typeArgs.tail))
    }
    reader.readEndArray()
    list.toList.asInstanceOf[V]
  }

  protected def readDocument[V](reader: BsonReader, decoderContext: DecoderContext, clazz: Class[V], typeArgs: List[Class[_]]): V = {
    val isCaseClass = clazzToCaseClassMap.getOrElse(clazz, false)
    if (isCaseClass) {
      registry.get(clazz).decode(reader, decoderContext)
    } else {
      val map = mutable.Map[String, Any]()
      val currentName = reader.getCurrentName
      reader.readStartDocument()
      while (reader.readBsonType ne BsonType.END_OF_DOCUMENT) {
        // Can't trust the order of fields in a map
        val name = reader.readName
        val fieldClazzTypeArgs = fieldTypeArgsMap.getOrElse(name, typeArgs)
        map += (name -> readValue(reader, decoderContext, fieldClazzTypeArgs.head, fieldClazzTypeArgs.tail))
      }
      reader.readEndDocument()
      map.toMap.asInstanceOf[V]
    }
  }

}
