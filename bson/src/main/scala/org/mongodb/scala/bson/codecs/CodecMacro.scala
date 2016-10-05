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

object CodecMacro {
  /**
   * Internal create codec implementation.
   */
  def createCodecNoArgs[T: c.WeakTypeTag](c: whitebox.Context)(): c.Expr[Codec[T]] = {
    import c.universe._
    createCodec[T](c)(c.Expr[CodecRegistry](
      q"""
         import org.mongodb.scala.bson.codecs.DEFAULT_CODEC_REGISTRY
         DEFAULT_CODEC_REGISTRY
      """
    )).asInstanceOf[c.Expr[Codec[T]]]
  }

  // scalastyle:off method.length
  def createCodec[T: c.WeakTypeTag](c: whitebox.Context)(codecRegistry: c.Expr[CodecRegistry]): c.Expr[Codec[T]] = {
    import c.universe._

    // Declared types
    val mainType = weakTypeOf[T]

    // Names
    val classTypeName = mainType.typeSymbol.name.toTypeName
    val caseClassName = TypeName(s"${classTypeName}MacroCodec")

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
          q"map += ($key -> classOf[${f.finalResultType}])"
      })

      q"""
          val map = Map[String, Class[_]]()
          ..$setFieldClasses
          map.toMap
        """
    }

    def createFieldTypeArgsMap = {
      val setTypeArgs = fields.map({
        case (f, name) =>
          val key = keyName(name)
          val clazz = f.typeSymbol
          q"""typeArgs += ($key -> {
                    val tpeArgs = ListBuffer.empty[Class[_]]
                    ..${f.typeArgs.map(x => q"tpeArgs += classOf[${x.finalResultType}]")}
                    tpeArgs.toList
                  })"""
      })

      q"""
          val typeArgs = Map[String, List[Class[_]]]()
          ..$setTypeArgs
         typeArgs.toMap
        """
    }

    c.Expr[Codec[T]](
      q"""
         import scala.collection.JavaConverters._
         import org.bson.{BsonReader, BsonType, BsonValue, BsonWriter}
         import org.bson.codecs.configuration.{ CodecRegistry, CodecRegistries }
         import org.bson.codecs.{ Encoder, Codec, DecoderContext, EncoderContext }
         import scala.collection.mutable.Map
         import scala.collection.mutable.ListBuffer

         case class $caseClassName(codecRegistry: CodecRegistry) extends Codec[$classTypeName] {
           private val fieldClassMap = $createFieldClassMap
           private val fieldTypeArgsMap = $createFieldTypeArgsMap
           private val registry = CodecRegistries.fromRegistries(List(codecRegistry, CodecRegistries.fromCodecs(this)).asJava)

           override def encode(writer: BsonWriter, value: $classTypeName, encoderContext: EncoderContext): Unit =
              writeValue(writer, value, encoderContext)

           override def decode(reader: BsonReader, decoderContext: DecoderContext): $classTypeName = {
              val map = Map[String, Any]()
              reader.readStartDocument()
               while (reader.readBsonType ne BsonType.END_OF_DOCUMENT) {
                  val name = reader.readName
                  val clazz = fieldClassMap.getOrElse(name, classOf[BsonValue])
                  val typeArgs = fieldTypeArgsMap.getOrElse(name, List.empty[Class[_]])
                  map += (name -> readValue(reader, decoderContext, clazz, typeArgs))
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

           private def readValue[T](reader: BsonReader, decoderContext: DecoderContext, clazz: Class[T], typeArgs: List[Class[_]]): T = {
              registry.get(clazz).decode(reader, decoderContext)
           }

        }

       ${caseClassName.toTermName}($codecRegistry).asInstanceOf[Codec[$mainType]]
       """
    )
  }
  // scalastyle:on method.length
}
