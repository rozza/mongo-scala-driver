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

import scala.collection.MapLike
import scala.reflect.macros.whitebox

import org.bson.codecs.Codec
import org.bson.codecs.configuration.CodecRegistry

private[codecs] object CaseClassCodec {
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
    val stringType = typeOf[String]
    val mapTypeSymbol = typeOf[MapLike[_, _, _]].typeSymbol

    // Names
    val codecName = TypeName(s"${mainType.typeSymbol.name.toTypeName}MacroCodec")

    // Type checkers
    def keyName(t: TermName): Literal = Literal(Constant(t.toString))
    def fields: List[(Type, TermName)] = mainType.members.sorted.filter(_.isMethod).map(_.asMethod).filter(_.isGetter)
      .map(m => (m.returnType.asSeenFrom(mainType, mainType.typeSymbol), m.name))
    def isMap(t: Type): Boolean = t.baseClasses.contains(mapTypeSymbol)
    def isCaseClass(t: Type): Boolean = t.typeSymbol.isClass && t.typeSymbol.asClass.isCaseClass
    def isOption(t: Type): Boolean = t.typeSymbol == definitions.OptionClass
    def isTuple(t: Type): Boolean = definitions.TupleClass.seq.contains(t.typeSymbol)
    def isSealed(t: Type): Boolean = t.typeSymbol.isClass && t.typeSymbol.asClass.isSealed
    def isCaseClassOrSealed(t: Type): Boolean = isCaseClass(t) || isSealed(t)

    // Primitives type map
    val primitiveTypesMap: Map[Type, Type] = Map(
      typeOf[Boolean] -> typeOf[java.lang.Boolean],
      typeOf[Byte] -> typeOf[java.lang.Byte],
      typeOf[Char] -> typeOf[java.lang.Character],
      typeOf[Double] -> typeOf[java.lang.Double],
      typeOf[Float] -> typeOf[java.lang.Float],
      typeOf[Int] -> typeOf[java.lang.Integer],
      typeOf[Long] -> typeOf[java.lang.Long],
      typeOf[Short] -> typeOf[java.lang.Short]
    )

    /**
     * Writes the Case Class fields and values to the BsonWriter
     */
    def writeValue: Tree = {
      val writeFields = fields.map({
        case (f, name) =>
          val key = keyName(name)
          f match {
            case optional if isOption(optional) => q"""
              if (value.$name.isDefined) {
                writer.writeName($key)
                this.writeValue(writer, value.$name.get, encoderContext)
              }"""
            case _ => q"""
              writer.writeName($key)
              this.writeValue(writer, value.$name, encoderContext)
              """
          }
      })
      q"""
        writer.writeStartDocument()
        ..$writeFields
        writer.writeEndDocument()
      """
    }

    /**
     * Returns a new instance of the case class using the [[fieldSetters]] method to set the name and values.
     */
    def getInstance: Tree = q"new ${mainType.resultType}(..$fieldSetters)"

    /**
     * Gets the name and value of fields for the case class.
     *
     * Uses the map which contains the decoded document.
     */
    def fieldSetters: List[Tree] = {
      fields.map({
        case (f, name) =>
          val key = keyName(name)
          f match {
            case optional if isOption(optional) => q"$name = map.get($key).asInstanceOf[$f]"
            case _ => q"$name = map($key).asInstanceOf[$f]"
          }
      })
    }

    /**
     * Flattens the type args for any given type.
     *
     * Removes the key field from Maps as they have to be strings.
     * Removes Option type as the Option value is wrapped automatically below.
     * Throws if the case class contains a Tuple
     *
     * @param t the type to flatten the arguments for
     * @return a list of the type arguments for the type
     */
    def flattenTypeArgs(t: Type): List[c.universe.Type] = {
      val typeArgs = if (isMap(t)) {
        if (t.typeArgs.head != stringType) c.abort(c.enclosingPosition, "Maps must contain string types for keys")
        t.typeArgs.tail
      } else {
        t.typeArgs
      }
      val types = t +: typeArgs.flatMap(x => flattenTypeArgs(x))
      if (types.exists(isTuple)) c.abort(c.enclosingPosition, "Tuples currently aren't supported in case classes")
      types.filter(x => !isOption(x)).map(x => primitiveTypesMap.getOrElse(x, x))
    }

    /**
     * Maps the given field names to type args for the values in the field
     *
     * ```
     *  addresses: Seq[Address] => (addresses, List[classOf[Seq], classOf[Address]])
     *  nestedAddresses: Seq[Seq[Address]] => (addresses, List[classOf[Seq], classOf[Seq], classOf[Address]])
     * ```
     *
     * @return a map of the field names with a list of the contain types
     */
    def createFieldTypeArgsMap = {
      val setTypeArgs = fields.map({
        case (f, name) =>
          val key = keyName(name)
          q"""
            typeArgs += ($key -> {
              val tpeArgs = mutable.ListBuffer.empty[Class[_]]
              ..${flattenTypeArgs(f).map(t => q"tpeArgs += classOf[${t.finalResultType}]")}
              tpeArgs.toList
            })"""
      })

      q"""
        val typeArgs = mutable.Map[String, List[Class[_]]]()
        ..$setTypeArgs
        typeArgs.toMap
      """
    }

    def createClazzToCaseClassMap = {
      val setClazzIsCaseClass = fields.map({
        case (f, name) => q"clazzIsCaseClass ++= ${flattenTypeArgs(f).map(t => q"(classOf[${t.finalResultType}], ${isCaseClassOrSealed(t)})")}"
      })

      q"""
        val clazzIsCaseClass = mutable.Map[Class[_], Boolean]()
        ..$setClazzIsCaseClass
        clazzIsCaseClass.toMap
      """
    }

    c.Expr[Codec[T]](
      q"""
        import scala.collection.JavaConverters._
        import org.bson.{BsonReader, BsonType, BsonValue, BsonWriter}
        import org.bson.codecs.configuration.{ CodecRegistry, CodecRegistries }
        import org.bson.codecs.{ Encoder, Codec, DecoderContext, EncoderContext }
        import scala.collection.mutable
        import scala.util.{ Failure, Success, Try }

        case class $codecName(codecRegistry: CodecRegistry) extends Codec[$mainType] {
          private val fieldTypeArgsMap = $createFieldTypeArgsMap
          private val clazzToCaseClassMap = $createClazzToCaseClassMap
          private val registry = CodecRegistries.fromRegistries(List(codecRegistry, CodecRegistries.fromCodecs(this)).asJava)

          override def encode(writer: BsonWriter, value: $mainType, encoderContext: EncoderContext): Unit =
             writeValue(writer, value, encoderContext)

          override def decode(reader: BsonReader, decoderContext: DecoderContext): $mainType = {
            val map = mutable.Map[String, Any]()
            reader.readStartDocument()
             while (reader.readBsonType ne BsonType.END_OF_DOCUMENT) {
               val name = reader.readName
               val typeArgs = fieldTypeArgsMap.getOrElse(name, List.empty[Class[BsonValue]])
               map += (name -> readValue(reader, decoderContext, typeArgs.head, typeArgs.tail))
            }
            reader.readEndDocument()

            $getInstance
          }

          override def getEncoderClass: Class[$mainType] = classOf[$mainType]

          private def writeValue[V](writer: BsonWriter, value: V, encoderContext: EncoderContext): Unit = {
            value match {
              case value: $mainType => $writeValue
              case _ =>
                val codec = registry.get(value.getClass).asInstanceOf[Encoder[V]]
                encoderContext.encodeWithChildContext(codec, writer, value)
            }
          }

          private def readValue[T](reader: BsonReader, decoderContext: DecoderContext, clazz: Class[T], typeArgs: List[Class[_]]): T = {
            val currentType = reader.getCurrentBsonType
            currentType match {
              case BsonType.DOCUMENT => readDocument(reader, decoderContext, clazz, typeArgs)
              case BsonType.ARRAY => readArray(reader, decoderContext, clazz, typeArgs)
              case _ => registry.get(clazz).decode(reader, decoderContext)
            }
          }

          private def readArray[T](reader: BsonReader, decoderContext: DecoderContext, clazz: Class[T], typeArgs: List[Class[_]]): T = {
            reader.readStartArray()
            val list = mutable.ListBuffer[Any]()
            while (reader.readBsonType ne BsonType.END_OF_DOCUMENT) {
              list.append(readValue(reader, decoderContext, typeArgs.head, typeArgs.tail))
            }
            reader.readEndArray()
            list.toList.asInstanceOf[T]
          }

          private def readDocument[T](reader: BsonReader, decoderContext: DecoderContext, clazz: Class[T], typeArgs: List[Class[_]]): T = {
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
              map.toMap.asInstanceOf[T]
            }
          }
        }
        ${codecName.toTermName}($codecRegistry)
      """
    )
  }
  // scalastyle:on method.length
}
