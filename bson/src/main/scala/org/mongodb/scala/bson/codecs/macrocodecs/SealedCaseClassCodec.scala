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

private[codecs] object SealedCaseClassCodec {

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
    val classTypeName = mainType.typeSymbol.name.toTypeName
    val codecName = TypeName(s"${classTypeName}SealedMacroCodec")

    // Type checkers
    def isCaseClass(t: Type): Boolean = t.typeSymbol.isClass && t.typeSymbol.asClass.isCaseClass
    def keyName(t: Type): Literal = Literal(Constant(t.toString))
    def keyNameTerm(t: TermName): Literal = Literal(Constant(t.toString))
    val subClasses: List[Type] = mainType.typeSymbol.asClass.knownDirectSubclasses.map(_.asClass.toType).filter(isCaseClass).toList
    val knownTypes = (mainType +: subClasses).reverse
    def fields: Map[Type, List[(Type, TermName)]] = knownTypes.map(t => (t, t.members.sorted.filter(_.isMethod).map(_.asMethod).filter(_.isGetter)
      .map(m => (m.returnType.asSeenFrom(t, t.typeSymbol), m.name)))).toMap
    def isMap(t: Type): Boolean = t.baseClasses.contains(mapTypeSymbol)
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
    def createFieldTypeArgsMap(fields: List[(Type, TermName)]) = {
      val setTypeArgs = fields.map({
        case (f, name) =>
          val key = keyNameTerm(name)
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

    /**
     * For each sealed class sets the Map of the given field names and their field types.
     */
    def createClassFieldTypeArgsMap = {
      val setClassFieldTypeArgs = fields.map(field =>
        q"""
            classFieldTypeArgs += (${keyName(field._1)} -> ${createFieldTypeArgsMap(field._2)})
        """)

      q"""
        val classFieldTypeArgs = mutable.Map[String, Map[String, List[Class[_]]]]()
        ..$setClassFieldTypeArgs
        classFieldTypeArgs.toMap
      """
    }

    def subClassesMap = {
      val setSubClasses = subClasses.map(t => q"subClassesMap += (${keyName(t)} -> classOf[${t.finalResultType}])")
      q"""
        val subClassesMap = mutable.Map[String, Class[_]]()
        ..$setSubClasses
        subClassesMap.toMap
      """
    }

    def clazzToCaseClassMap = {
      val setClazzToCaseClassMap = fields.map({
        case (f, name) => q"clazzToCaseClassMap ++= ${flattenTypeArgs(f).map(t => q"(classOf[${t.finalResultType}], ${isCaseClassOrSealed(t)})")}"
      })

      q"""
        val clazzToCaseClassMap = mutable.Map[Class[_], Boolean]()
        ..$setClazzToCaseClassMap
        clazzToCaseClassMap.toMap
      """
    }

    def writeClassValues(fields: List[(Type, TermName)]): List[Tree] = {
      fields.map({
        case (f, name) =>
          val key = keyNameTerm(name)
          f match {
            case optional if isOption(optional) => q"""
              if (instanceValue.$name.isDefined) {
                writer.writeName($key)
                this.writeValue(writer, instanceValue.$name.get, encoderContext)
              }"""
            case _ => q"""
              val localKey = $key
              val localVal = instanceValue.$name
              writer.writeName($key)
              this.writeValue(writer, instanceValue.$name, encoderContext)
              """
          }
      })
    }

    /**
     * Writes the Case Class fields and values to the BsonWriter
     */
    def writeValue: Tree = {
      val cases: Seq[Tree] = {
        fields.map(field => cq"""
          ${keyName(field._1)} =>
            val instanceValue = value.asInstanceOf[${field._1}]
            ..${writeClassValues(field._2)}
          """).toSeq
      }

      q"""
        writer.writeStartDocument()
        writer.writeName(this.classFieldName)
        this.writeValue(writer, className, encoderContext)
        className match { case ..$cases }
        writer.writeEndDocument()
      """
    }

    def fieldSetters(fields: List[(Type, TermName)]) = {
      fields.map({
        case (f, name) =>
          val key = keyNameTerm(name)
          f match {
            case optional if isOption(optional) => q"$name = map.get($key).asInstanceOf[$f]"
            case _ => q"$name = map($key).asInstanceOf[$f]"
          }
      })
    }

    def getInstance = {
      val cases = knownTypes.map { st =>
        cq"${keyName(st)} => new $st(..${fieldSetters(fields(st))})"
      } :+ cq"""_ => throw new UnsupportedOperationException("Unexpected class type: " + className)"""
      q"className match { case ..$cases }"
    }

    c.Expr[Codec[T]](
      q"""
        import scala.collection.JavaConverters._
        import scala.collection.mutable
        import org.bson.{BsonReader, BsonType, BsonValue, BsonWriter}
        import org.bson.codecs.configuration.{ CodecRegistry, CodecRegistries, CodecConfigurationException }
        import org.bson.codecs.{ Encoder, Codec, DecoderContext, EncoderContext }
        import org.mongodb.scala.bson.codecs.Macros

        case class $codecName(codecRegistry: CodecRegistry) extends Codec[$classTypeName] {
          private val classFieldName = "_cls"
          private val subClassesMap = $subClassesMap
          private val subClassesMapInv = subClassesMap.map(_.swap)
          private val clazzToCaseClassMap = $clazzToCaseClassMap
          private val classFieldTypeArgsMap = $createClassFieldTypeArgsMap
          private val registry = CodecRegistries.fromRegistries((List(codecRegistry, CodecRegistries.fromCodecs(this))).asJava)

          override def encode(writer: BsonWriter, value: $mainType, encoderContext: EncoderContext): Unit =
            writeValue(writer, value, encoderContext)

          override def decode(reader: BsonReader, decoderContext: DecoderContext): $mainType = {
            // Find the class name
            reader.mark()
            reader.readStartDocument()
            var optionalClassName: Option[String] = None
            while (optionalClassName.isEmpty && (reader.readBsonType ne BsonType.END_OF_DOCUMENT)) {
              val name = reader.readName
              if (name == classFieldName) {
                optionalClassName = Some(codecRegistry.get(classOf[String]).decode(reader, decoderContext))
              } else {
                reader.skipValue
              }
            }
            reader.reset()

            // Validate the class name
            if (optionalClassName.isEmpty) {
              throw new CodecConfigurationException(s"Could not decode sealed case class. Missing '$$classFieldName' field.")
            }

            val className = optionalClassName.get
            if (!subClassesMap.contains(className)) {
              throw new CodecConfigurationException(s"Could not decode sealed case class, unknown class $${className}.")
            }

            val fieldTypeArgsMap = classFieldTypeArgsMap(className)
            val map = mutable.Map[String, Any]()
            reader.readStartDocument()
             while (reader.readBsonType ne BsonType.END_OF_DOCUMENT) {
               val name = reader.readName
               val typeArgs = if (name == classFieldName) List(classOf[String]) else fieldTypeArgsMap.getOrElse(name, List.empty[Class[_]])
               map += (name -> readValue(reader, decoderContext, typeArgs.head, typeArgs.tail, fieldTypeArgsMap))
            }
            reader.readEndDocument()

            $getInstance
          }

          override def getEncoderClass: Class[$mainType] = classOf[$mainType]

          private def writeValue[V](writer: BsonWriter, value: V, encoderContext: EncoderContext): Unit = {
            val clazz = value.getClass
            subClassesMapInv.get(clazz) match {
              case Some(className) => $writeValue
              case None =>
                val codec = registry.get(clazz).asInstanceOf[Encoder[V]]
                encoderContext.encodeWithChildContext(codec, writer, value)
            }
          }

          private def readValue[T](reader: BsonReader, decoderContext: DecoderContext, clazz: Class[T], typeArgs: List[Class[_]],
                                   fieldTypeArgsMap: Map[String, List[Class[_]]]): T = {
            val currentType = reader.getCurrentBsonType
            currentType match {
              case BsonType.DOCUMENT => readDocument(reader, decoderContext, clazz, typeArgs, fieldTypeArgsMap)
              case BsonType.ARRAY => readArray(reader, decoderContext, clazz, typeArgs, fieldTypeArgsMap)
              case _ => registry.get(clazz).decode(reader, decoderContext)
            }
          }

          private def readArray[T](reader: BsonReader, decoderContext: DecoderContext, clazz: Class[T], typeArgs: List[Class[_]],
                                   fieldTypeArgsMap: Map[String, List[Class[_]]]): T = {
            reader.readStartArray()
            val list = mutable.ListBuffer[Any]()
            while (reader.readBsonType ne BsonType.END_OF_DOCUMENT) {
              list.append(readValue(reader, decoderContext, typeArgs.head, typeArgs.tail, fieldTypeArgsMap))
            }
            reader.readEndArray()
            list.toList.asInstanceOf[T]
          }

          private def readDocument[T](reader: BsonReader, decoderContext: DecoderContext, clazz: Class[T], typeArgs: List[Class[_]],
                                      fieldTypeArgsMap: Map[String, List[Class[_]]]): T = {
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
              map.toMap.asInstanceOf[T]
            }
          }
      }

      ${codecName.toTermName}($codecRegistry).asInstanceOf[Codec[$mainType]]
      """
    )
  }
  // scalastyle:on method.length
}
