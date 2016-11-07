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

import scala.collection.MapLike
import scala.reflect.macros.whitebox

import org.bson.codecs.Codec
import org.bson.codecs.configuration.CodecRegistry

object SealedClassCodecMacro {
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
    val mapType = typeOf[MapLike[_, _, _]].typeSymbol

    // Names
    val baseClassTypeName = weakTypeOf[T].typeSymbol.name.toTypeName
    val caseClassName = TypeName(s"${baseClassTypeName}SealedMacroCodec")
    val classFieldName = "_cls"

    // Type checkers
    def isMap(t: Type): Boolean = t.baseClasses.contains(mapType)
    def isCaseClass(t: Type): Boolean = t.typeSymbol.isClass && t.typeSymbol.asClass.isCaseClass
    def isOption(t: Type): Boolean = t.typeSymbol == definitions.OptionClass
    def isTuple(t: Type): Boolean = definitions.TupleClass.seq.contains(t.typeSymbol)

    // Helpers
    def keyName(t: TermName): Literal = Literal(Constant(t.toString))
    def subClasses(t: Type): List[Type] = t.typeSymbol.asClass.knownDirectSubclasses.map(_.asClass.toType).filter(isCaseClass).toList
    def typeDecodedName(t: Type): String = t.typeSymbol.asClass.name.decodedName.toString
    val knownTypes = mainType +: subClasses(mainType)
    def fields: Map[Type, List[(Type, TermName)]] = knownTypes.map(t => (t, t.members.sorted.filter(_.isMethod).map(_.asMethod).filter(_.isGetter)
      .map(m => (m.returnType.asSeenFrom(t, t.typeSymbol), m.name)))).toMap

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

    def writeValue(fields: List[(Type, TermName)]): Tree = {
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

    def getInstance(map: Tree) = {
      val subClassName = q"$map($classFieldName)"
      val cases = knownTypes.map { st =>
        cq"${typeDecodedName(st)} => new $st(..${fieldSetters(fields(st))})"
      } :+ cq"""_ => throw new UnsupportedOperationException("Unexpected class type: " + $subClassName)"""
      q"map($classFieldName) match { case ..$cases }"
    }

    def fieldSetters(fields: List[(Type, TermName)]) = {
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
     * Flattens the type args for a given type.
     *
     * Removes the key field from Maps as they should be maps.
     * Removes Option type as the Option value is wrapped automatically below.
     * Throws if the case class contains a Tuple
     */
    def flattenTypeArgs(t: Type): List[c.universe.Type] = {
      val typeArgs = if (isMap(t)) t.typeArgs.tail else t.typeArgs
      val types = t +: typeArgs.flatMap(x => flattenTypeArgs(x))
      if (types.exists(isTuple)) c.abort(c.enclosingPosition, "Tuples currently aren't supported in case classes")
      types.filter(x => !isOption(x)).map(x => primitiveTypesMap.getOrElse(x, x))
    }

    def getClasses(types: List[Type]) = {
      val classes = types.map(t => q"classOf[$t]")
      q"List[Class[_]](...$classes)"
    }

    def createFieldTypeArgsMap(fieldsTypeAndName: List[(Type, TermName)]) = {
      val x: List[(c.universe.Literal, List[Type])] = fieldsTypeAndName.map(tn =>
        (keyName(tn._2), flattenTypeArgs(tn._1)))
      x.map(kv => q"""(${kv._1} -> ${getClasses(kv._2)})""")
    }

    def createSealedClassFieldTypeArgsMap = {
      val setTypeArgs = fields.map(kv =>
        q"""
          typeArgs += (${typeDecodedName(kv._1)} -> ${createFieldTypeArgsMap(kv._2)})
         """)

      q"""
        val typeArgs = Map[String, Map[String, List[Class[_]]]]()
        ..$setTypeArgs
        typeArgs.toMap
      """
    }

    def createClazzToCaseClassMap = {
      val setClazzIsCaseClass = fields.map({
        case (f, name) => q"clazzIsCaseClass ++= ${flattenTypeArgs(f).map(t => q"(classOf[${t.finalResultType}], ${isCaseClass(t)})")}"
      })

      q"""
        val clazzIsCaseClass = Map[Class[_], Boolean]()
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
        import scala.collection.mutable.Map
        import scala.collection.mutable.ListBuffer
        import scala.util.{ Failure, Success, Try }

        case class $caseClassName(codecRegistry: CodecRegistry) extends Codec[$baseClassTypeName] {
          private val fieldTypeArgsMap = $createSealedClassFieldTypeArgsMap
          private val clazzToCaseClassMap = $createClazzToCaseClassMap
          private val registry = CodecRegistries.fromRegistries(List(codecRegistry, CodecRegistries.fromCodecs(this)).asJava)

          override def encode(writer: BsonWriter, value: $baseClassTypeName, encoderContext: EncoderContext): Unit =
             writeValue(writer, value, encoderContext)

          override def decode(reader: BsonReader, decoderContext: DecoderContext): $baseClassTypeName = {

            // Find the class name
            reader.mark()
            reader.readStartDocument()
            var className: Option[String] = None
            while (!className.isDefined && reader.readBsonType ne BsonType.END_OF_DOCUMENT) {
              val name = reader.readName
              if (name == $classFieldName) {
                className = Some(readValue(reader, decoderContext, classOf[String], List()))
              } else {
                reader.skipValue
              }
            }
            reader.reset()

            // Validate the class name
            // TODO BETTER ERROR MESSAGES
            if (className.isEmpty) {
              throw new CodecConfigurationException("Could not decode sealed class. Missing 'classFieldName' field.")
            } else if (!fieldTypeArgsMap.contains(className)) {
              throw new CodecConfigurationException(s"Could not decode sealed class, unknown class $$className.")
            }

            // Decode
            val fieldTypeArgs = fieldTypeArgsMap(className)
            val map = Map[String, Any]()
            reader.readStartDocument()
             while (reader.readBsonType ne BsonType.END_OF_DOCUMENT) {
               val name = reader.readName
               val typeArgs = fieldTypeArgsMap.getOrElse(name, List.empty[Class[BsonValue]])
               println(s">>> $$name :: $$typeArgs.head : $$typeArgs.tail")
               map += (name -> readValue(reader, decoderContext, typeArgs.head, typeArgs.tail))
            }
            reader.readEndDocument()

            ${getInstance(q"map")}
          }

          override def getEncoderClass: Class[$baseClassTypeName] = classOf[$baseClassTypeName]

          private def writeValue[V](writer: BsonWriter, value: V, encoderContext: EncoderContext): Unit = {
            value match {
              case value: $baseClassTypeName => ${writeValue(fields(mainType))}
              case _ =>
                val codec = registry.get(value.getClass).asInstanceOf[Encoder[V]]
                encoderContext.encodeWithChildContext(codec, writer, value)
            }
          }

          private def readValue[T](reader: BsonReader, decoderContext: DecoderContext, clazz: Class[T], typeArgs: List[Class[_]]): T = {
            val currentType = reader.getCurrentBsonType
            println(s"READING VALUE: $$clazz :: $$currentType")
            currentType match {
              case BsonType.DOCUMENT => readDocument(reader, decoderContext, clazz, typeArgs)
              case BsonType.ARRAY => readArray(reader, decoderContext, clazz, typeArgs)
              case _ => registry.get(clazz).decode(reader, decoderContext)
            }
          }

          private def readArray[T](reader: BsonReader, decoderContext: DecoderContext, clazz: Class[T], typeArgs: List[Class[_]]): T = {
            println(s"READING ARRAY: $$clazz")
            reader.readStartArray()
            val list = ListBuffer[Any]()
            while (reader.readBsonType ne BsonType.END_OF_DOCUMENT) {
              list.append(readValue(reader, decoderContext, typeArgs.head, typeArgs.tail))
            }
            reader.readEndArray()
            list.toList.asInstanceOf[T]
          }

        private def readDocument[T](reader: BsonReader, decoderContext: DecoderContext, clazz: Class[T], typeArgs: List[Class[_]]): T = {
          val isCaseClass = clazzToCaseClassMap.getOrElse(clazz, false)
          println(s"READING DOCUMENT: $$clazz -> isCC: $$isCaseClass")
          if (isCaseClass) {
            registry.get(clazz).decode(reader, decoderContext)
          } else {
            val map = Map[String, Any]()
            val currentName = reader.getCurrentName
            println(s" READING currentName: $$currentName")
            reader.readStartDocument()
            while (reader.readBsonType ne BsonType.END_OF_DOCUMENT) {
              // Can't trust the order of fields in a map
              val name = reader.readName
              println(s" READING FIELD: $$name")
              val fieldClazzTypeArgs = fieldTypeArgsMap.getOrElse(name, typeArgs)
              map += (name -> readValue(reader, decoderContext, fieldClazzTypeArgs.head, fieldClazzTypeArgs.tail))
            }
            reader.readEndDocument()
            map.toMap.asInstanceOf[T]
          }
        }
        }

       ${caseClassName.toTermName}($codecRegistry).asInstanceOf[Codec[$baseClassTypeName]]
       """
    )
  }
  // scalastyle:on method.length
}
