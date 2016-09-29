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

import scala.util.{ Failure, Success, Try }

import org.bson.codecs.configuration.{ CodecConfigurationException, CodecRegistry }
import org.bson.codecs.{ Codec, DecoderContext, EncoderContext }
import org.bson.{ BsonReader, BsonWriter }

trait MacroMapCodecBase[T] extends Codec[T] {
  val codecRegistry: CodecRegistry
  lazy val mapCodec = Option(codecRegistry.get(classOf[Map[String, _]])) match {
    case Some(codec) => codec
    case None => throw new CodecConfigurationException(
      s"""Macro based codecs internally require a Codec that produce Maps.
         |Please ensure the IterableCodec is included in the codec registry.
       """.stripMargin
    )
  }

  def getInstance(map: Map[String, _ <: Any]): T
  def getMap(instance: T): Map[String, _ <: Any]

  override def decode(reader: BsonReader, decoderContext: DecoderContext): T = {
    val map = mapCodec.decode(reader, decoderContext)
    Try(getInstance(map)) match {
      case Failure(ex) => throw new CodecConfigurationException(s"Unable to decode and create the instance: ${ex.getMessage}")
      case Success(instance) => instance
    }
  }

  override def encode(writer: BsonWriter, value: T, encoderContext: EncoderContext): Unit = {
    val asMap = getMap(value)
    Try(mapCodec.encode(writer, asMap, encoderContext)) match {
      case Failure(ex) => throw new CodecConfigurationException(s"Unable to convert and encode $value: ${ex.getMessage}")
      case _ =>
    }
  }

}

