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

package org.mongodb.scala.bson.codecs

import scala.reflect.macros.whitebox

import org.bson.codecs.Codec
import org.bson.codecs.configuration.CodecRegistry

object MacroImpl {
  /**
   * Internal create codec implementation.
   */
  def createCodecImplNoArgs[T: c.WeakTypeTag](c: whitebox.Context)(): c.Expr[Codec[T]] = {
    import c.universe._
    createCodecImpl[T](c)(c.Expr[CodecRegistry](
      q"""
         import org.mongodb.scala.bson.codecs.DEFAULT_CODEC_REGISTRY
         DEFAULT_CODEC_REGISTRY
      """
    )).asInstanceOf[c.Expr[Codec[T]]]
  }

  // scalastyle:off method.length cyclomatic.complexity
  def createCodecImpl[T: c.WeakTypeTag](c: whitebox.Context)(codecRegistry: c.Expr[CodecRegistry]): c.Expr[Codec[T]] = {
    import c.universe._

    // Declared types
    val mainType = weakTypeOf[T]

    // Names
    val classTypeName = mainType.typeSymbol.name.toTypeName

    // Type checkers
    def keyName(t: TermName): Literal = Literal(Constant(t.toString))
    def fields: List[(Type, TermName)] = mainType.members.sorted.filter(_.isMethod).map(_.asMethod).filter(_.isGetter)
      .map(m => (m.returnType.asSeenFrom(mainType, mainType.typeSymbol), m.name))

    def writeValue: Tree = {
      val writeFields = fields.map({
        case (f, name) =>
          val key = keyName(name)
          val fieldValue = q"value.$name"
          q"""
             writer.writeName($key)
             this.writeValue(writer, $fieldValue, encoderContext)
           """
      })
      q"""
          writer.writeStartDocument()
          ..$writeFields
          writer.writeEndDocument()
        """
    }

    def getInstance = q"new ${mainType.resultType}(..$fieldSetters)"

    def fieldSetters = {
      fields.map({
        case (f, name) =>
          val key = keyName(name)
          q"$name = map($key).asInstanceOf[$f]"
      })
    }

    def createFieldClassMap = {
      val setFieldClasses = fields.map({
        case (f, name) =>
          val key = keyName(name)
          val clazz = f.typeSymbol
          q"map += ($key -> classOf[$clazz])"
      })
      q"""
          val map = Map[String, Class[_]]()
          ..$setFieldClasses
          map.toMap
        """
    }

    c.Expr[Codec[T]](
      q"""
         import org.bson.{BsonReader, BsonType, BsonValue, BsonWriter}
         import org.bson.codecs.configuration.CodecRegistry
         import org.bson.codecs.{ Encoder, Codec, DecoderContext, EncoderContext }
         import scala.collection.mutable.Map

         new Codec[$classTypeName] {
           val registry = $codecRegistry
           val fieldClassMap = $createFieldClassMap

           override def encode(writer: BsonWriter, value: $classTypeName, encoderContext: EncoderContext): Unit =
              writeValue(writer, value, encoderContext)

           override def decode(reader: BsonReader, decoderContext: DecoderContext): $classTypeName = {
              val map = Map[String, Any]()
              reader.readStartDocument()
               while (reader.readBsonType ne BsonType.END_OF_DOCUMENT) {
                  val name = reader.readName
                  val clazz = fieldClassMap.getOrElse(name, classOf[BsonValue])
                  map += (name -> readValue(reader, decoderContext, clazz))
              }
              reader.readEndDocument()
              $getInstance
           }

           override def getEncoderClass: Class[$classTypeName] = classOf[$classTypeName]

           private def writeValue[V](writer: BsonWriter, value: V, encoderContext: EncoderContext): Unit = {
             value match {
               case value: $classTypeName => $writeValue
               case _ =>
                 val codec = registry.get(value.getClass).asInstanceOf[Encoder[V]]
                 encoderContext.encodeWithChildContext(codec, writer, value)
             }
           }

           private def readValue[T](reader: BsonReader, decoderContext: DecoderContext, clazz: Class[T]): T =
              registry.get(clazz).decode(reader, decoderContext)

        }
       """
    )
  }
}
