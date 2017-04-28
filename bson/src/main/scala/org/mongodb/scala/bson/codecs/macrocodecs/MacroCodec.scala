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

import org.bson.codecs.configuration.{ CodecConfigurationException, CodecRegistries, CodecRegistry }
import org.bson.{ BsonReader, BsonType, BsonValue, BsonWriter }
import org.bson.codecs.{ Codec, DecoderContext, Encoder, EncoderContext }

import org.mongodb.scala.bson.BsonNull

/**
 *
 * @tparam T the case class type for the codec
 * @since 2.0
 */
trait MacroCodec[T] extends Codec[T] {

  /**
   * Creates a `Map[String, Class[_]]` mapping the case class name and the type.
   */
  val caseClassesMap: Map[String, Class[_]]

  /**
   * Creates a `Map[Class[_], Boolean]` mapping field types to a boolean representing if they are a case class.
   */
  val classToCaseClassMap: Map[Class[_], Boolean]

  /**
   * A nested map of case class name to a Map of the given field names and a list of the field types.
   */
  val classFieldTypeArgsMap: Map[String, Map[String, List[Class[_]]]]

  /**
   * The case class type for the codec
   */
  val encoderClass: Class[T]

  /**
   * The `CodecRegistry` for use with the codec
   */
  val codecRegistry: CodecRegistry

  /**
   * Creates a new instance of the case class with the provided data
   *
   * @param className the name of the class to be instantiated
   * @param fieldsData the Map of data for the class
   * @return the new instance of the class
   */
  def getInstance(className: String, fieldsData: Map[String, Any]): T

  /**
   * The method that writes the data for the case class
   *
   * @param className the name of the current case class being written
   * @param writer the `BsonWriter`
   * @param value the value to the case class
   * @param encoderContext the `EncoderContext`
   */
  def writeCaseClassData(className: String, writer: BsonWriter, value: T, encoderContext: EncoderContext): Unit

  /**
   * The field used to save the class name when saving sealed case classes.
   */
  val classFieldName = "_t"
  lazy val hasClassFieldName: Boolean = caseClassesMap.size > 1
  lazy val caseClassesMapInv: Map[Class[_], String] = caseClassesMap.map(_.swap)
  protected val registry: CodecRegistry = CodecRegistries.fromRegistries(List(codecRegistry, CodecRegistries.fromCodecs(this)).asJava)
  protected val unknownTypeArgs: List[Class[BsonValue]] = List[Class[BsonValue]](classOf[BsonValue])
  protected val bsonNull = BsonNull()

  override def encode(writer: BsonWriter, value: T, encoderContext: EncoderContext): Unit = {
    if (value == null) { // scalastyle:ignore
      throw new CodecConfigurationException(s"Invalid value for $encoderClass found a `null` value.")
    }
    writeValue(writer, value, encoderContext)
  }

  override def decode(reader: BsonReader, decoderContext: DecoderContext): T = {
    val className = getClassname(reader, decoderContext)
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

  protected def getClassname(reader: BsonReader, decoderContext: DecoderContext): String = {
    if (hasClassFieldName) {
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
      if (!caseClassesMap.contains(className)) {
        throw new CodecConfigurationException(s"Could not decode sealed case class, unknown class $className.")
      }
      className
    } else {
      caseClassesMap.head._1
    }
  }

  protected def writeClassFieldName(writer: BsonWriter, className: String, encoderContext: EncoderContext): Unit = {
    if (hasClassFieldName) {
      writer.writeName(classFieldName)
      this.writeValue(writer, className, encoderContext)
    }
  }

  protected def writeFieldValue[V](fieldName: String, writer: BsonWriter, value: V, encoderContext: EncoderContext): Unit = {
    if (value == null) { // scalastyle:ignore
      throw new CodecConfigurationException(s"Invalid value for $fieldName found a `null` value.")
    }
    writeValue(writer, value, encoderContext)
  }

  protected def writeValue[V](writer: BsonWriter, value: V, encoderContext: EncoderContext): Unit = {
    val clazz = value.getClass
    caseClassesMapInv.get(clazz) match {
      case Some(className) => writeCaseClassData(className: String, writer: BsonWriter, value.asInstanceOf[T], encoderContext: EncoderContext)
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
    val isCaseClass = classToCaseClassMap.getOrElse(clazz, false)
    if (isCaseClass) {
      registry.get(clazz).decode(reader, decoderContext)
    } else {
      val map = mutable.Map[String, Any]()
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
