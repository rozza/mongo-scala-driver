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

import scala.annotation.tailrec
import scala.collection.MapLike
import scala.reflect.macros.whitebox

import org.bson.codecs.Codec
import org.bson.codecs.configuration.CodecRegistry

private object MacroDocumentImpl {

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
    val iterableType = typeOf[Iterable[_]].typeSymbol
    val optionType = typeOf[Option[_]].typeSymbol
    val mapType = typeOf[MapLike[_, _, _]].typeSymbol

    // Names
    val classTypeName = mainType.typeSymbol.name.toTypeName
    val className = classTypeName.toString
    val classFieldName = "_cls"

    // Type checkers
    def isCaseClass(t: Type): Boolean = t.typeSymbol.isClass && t.typeSymbol.asClass.isCaseClass
    def isIterable(t: Type): Boolean = t.baseClasses.contains(iterableType) && !isMap(t)
    @tailrec
    def isIterableCaseClass(t: Type): Boolean = isIterable(t) && (isCaseClass(t.typeArgs.head) || isIterableCaseClass(t.typeArgs.head))
    @tailrec
    def isIterableSealedClass(t: Type): Boolean = isIterable(t) && (isSealed(t.typeArgs.head) || isIterableSealedClass(t.typeArgs.head))
    def isMap(t: Type): Boolean = t.baseClasses.contains(mapType)
    def isOption(t: Type): Boolean = t.baseClasses.contains(optionType)
    @tailrec
    def isOptionCaseClass(t: Type): Boolean = isOption(t) && (isCaseClass(t.typeArgs.head) || isOptionCaseClass(t.typeArgs.head))
    def isSealed(t: Type): Boolean = t.typeSymbol.isClass && t.typeSymbol.asClass.isSealed && getSubClasses(t).nonEmpty && !isOption(t)

    // Type Fetchers
    @tailrec
    def getIterableSealedClass(t: Type, depth: Int = -1): (Type, Int) = {
      if (isSealed(t)) {
        (t, depth)
      } else {
        getIterableSealedClass(t.typeArgs.head, depth + 1)
      }
    }
    @tailrec
    def getIterableSealedClasses(t: Type, depth: Int = -1): (List[Type], Int) = {
      if (isSealed(t)) {
        (getSubClasses(t), depth)
      } else {
        getIterableSealedClasses(t.typeArgs.head, depth + 1)
      }
    }
    @tailrec
    def getIterableCaseClass(t: Type, depth: Int = 0): (Type, Int) = {
      val nestedType = t.typeArgs.head
      if (isCaseClass(nestedType)) {
        (nestedType, depth)
      } else {
        getIterableCaseClass(nestedType, depth + 1)
      }
    }

    def getOptionCaseClass(t: Type): Type = t.typeArgs.head
    def getSubClasses(t: Type): List[Type] = t.typeSymbol.asClass.knownDirectSubclasses.map(_.asClass.toType).filter(isCaseClass).toList

    def keyName(t: TermName): Literal = Literal(Constant(t.toString))
    def fieldName(t: String, t1: TermName): String = Seq(t, t1.toString.capitalize).filter(_.nonEmpty).mkString("").stripPrefix(s"$className")
    def getFields(t: Type): List[(Type, TermName)] = {
      t.members.sorted.filter(_.isMethod).map(_.asMethod).filter(_.isGetter).map(m => (m.returnType.asSeenFrom(t, t.typeSymbol), m.name))
    }

    def findCaseClasses(t: Type, n: String, previouslySeen: List[Type] = List.empty[Type]): List[(Type, String)] = {
      val seen: List[c.universe.Type] = previouslySeen :+ t
      isIterable(t) match {
        case true =>
          if (previouslySeen.contains(t)) {
            List.empty[(Type, String)]
          } else {
            t.typeArgs.flatMap(findCaseClasses(_, n, seen))
          }
        case false =>
          getFields(t).flatMap({
            case (f, name) =>
              val expandedName = fieldName(n, f.typeSymbol.name.toTermName)
              f match {
                case seq if isIterableSealedClass(seq) =>
                  val (sealedClass, _) = getIterableSealedClass(seq)
                  val (childCaseClasses, _) = getIterableSealedClasses(seq)
                  (sealedClass, fieldName(n, sealedClass.typeSymbol.name.toTermName)) :: childCaseClasses.flatMap(cls => {
                    val expandedIterableName = fieldName(n, cls.typeSymbol.name.toTermName)
                    if (seen.contains(cls)) {
                      List.empty[(Type, String)]
                    } else {
                      (cls, expandedIterableName) :: seq.typeArgs.flatMap(i => findCaseClasses(i, expandedIterableName, seen :+ cls))
                    }
                  })
                case seq if isIterableCaseClass(seq) =>
                  val (cls, _) = getIterableCaseClass(seq)
                  val expandedIterableName = fieldName(n, cls.typeSymbol.name.toTermName)
                  if (seen.contains(cls)) {
                    List.empty[(Type, String)]
                  } else {
                    (cls, expandedIterableName) :: seq.typeArgs.flatMap(i => findCaseClasses(i, expandedIterableName, seen :+ cls))
                  }
                case opt if isOptionCaseClass(opt) =>
                  val cls = getOptionCaseClass(opt)
                  val expandedOptionalName = fieldName(n, cls.typeSymbol.name.toTermName)
                  (cls, expandedOptionalName) :: findCaseClasses(opt, expandedOptionalName, seen :+ cls)
                case adt if isSealed(adt) =>
                  if (seen.contains(adt)) {
                    List.empty[(Type, String)]
                  } else {
                    (adt, expandedName) :: getSubClasses(adt).flatMap(cls =>
                      (cls, fieldName(n, cls.typeSymbol.name.toTermName)) :: findCaseClasses(cls, expandedName, seen :+ adt))
                  }
                case nested if isIterable(nested) => nested.typeArgs.flatMap(i => findCaseClasses(i, expandedName, seen))
                case field if isCaseClass(field) => (field, expandedName) :: findCaseClasses(field, expandedName, seen)
                case field => findCaseClasses(field, fieldName(n, name), seen)
              }
          })
      }
    }

    // Returns the document version of the case class
    def getTypeAsDocument(t: c.Type, fName: String): Tree = {
      if (isSealed(t)) {
        getSealedInstanceAsDocument(t, None)
      } else {
        val fieldPutOps = getFields(t).map({
          case (f, name) =>
            val key = keyName(name)
            val methodName = fieldName(fName, f.typeSymbol.name.toTermName)
            val setterMethodName = getDocumentMethodName(methodName)
            f match {
              case adt if isSealed(adt) => q"""document.put($key, ${getSealedInstanceAsDocument(adt, Some(name))})"""
              case cls if isCaseClass(cls) => q"document.put($key, $setterMethodName(instance.$name))"
              case seq if isIterableCaseClass(seq) =>
                val (cls, depth) = getIterableCaseClass(seq)
                val expandedMethodName = fieldName(fName, cls.typeSymbol.name.toTermName)
                q"""document.put($key, nestedToJava(instance.$name, $depth, ${getDocumentMethodName(expandedMethodName)}))"""
              case seq if isIterableSealedClass(seq) =>
                val (cls, depth) = getIterableSealedClass(seq)
                val expandedMethodName = fieldName(fName, cls.typeSymbol.name.toTermName)
                q"""document.put($key, nestedToJava(instance.$name, $depth, ${getDocumentMethodName(expandedMethodName)}))"""
              case seq if isIterable(seq) =>
                q"""document.put($key, toJava(instance.$name))"""
              case opt if isOptionCaseClass(opt) =>
                val expandedMethodName = fieldName(fName, getOptionCaseClass(opt).typeSymbol.name.toTermName)
                q"""
                if (instance.$name.isDefined) {
                  document.put($key, ${getDocumentMethodName(expandedMethodName)}(instance.$name.get))
                }
                """
              case optional if isOption(optional) =>
                q"""
                  if (instance.$name.isDefined) {
                    document.put($key, instance.$name.get)
                  }
                """
              case _ => q"document.put($key, instance.$name)"
            }
        }) :+ putCaseClassName(t)
        q"""
           val document = new org.bson.Document()
            ..$fieldPutOps
            document
         """
      }
    }
    def getSealedInstanceAsDocument(t: Type, name: Option[TermName]): Tree = {
      val cases = getSubClasses(t).map { st =>
        val methodName = fieldName("", st.typeSymbol.name.toTermName)
        cq"v: ${st.resultType} => ${getDocumentMethodName(methodName)}(v)"
      } :+ cq"""v: Any => throw new CodecConfigurationException("Unexpected class type: " + v)"""

      name match {
        case Some(v) => q"instance.$v match { case ..$cases }"
        case None => q"instance match { case ..$cases }"
      }
    }
    def putCaseClassName(t: Type): Tree = {
      t.baseClasses.map(m => m.asType.toType).exists(isSealed) match {
        case true => q"""document.put($classFieldName, ${t.typeSymbol.name.decodedName.toString})"""
        case false => q""
      }
    }

    // Gets all the classes that the case class entails
    lazy val allClasses: List[(Type, String)] = {
      val cc = (mainType, className) :: findCaseClasses(mainType, className)
      val subCC = getSubClasses(mainType).flatMap(st => {
        val sn = st.typeSymbol.name.decodedName.toString
        (st, sn) :: findCaseClasses(st, sn)
      })
      (cc ::: subCC).distinct
    }
    def getMethodName(prefix: String)(fieldName: String): TermName = TermName(s"get$prefix$fieldName")
    def getInstanceMethodName(fieldName: String): TermName = getMethodName("Ins")(fieldName)
    def getDocumentMethodName(fieldName: String): TermName = getMethodName("Doc")(fieldName)
    def getDocumentMethod(t: Type, fName: String): Tree = {
      q"""
          def ${getDocumentMethodName(fName)}(instance: ${t.typeSymbol}[..${t.typeArgs}]): org.bson.Document = {
            ${getTypeAsDocument(t, fName)}
         }
       """
    }
    val getDocumentMethods = for (c <- allClasses) yield getDocumentMethod(c._1, c._2)

    def getInstanceOfSealedClass(t: Type, doc: Tree): Tree = {
      val subClassName = q"$doc.getString($classFieldName)"
      val cases = getSubClasses(t).map { st =>
        val methodName = fieldName("", st.typeSymbol.name.toTermName)
        cq"$methodName => ${getInstanceMethodName(methodName)}($doc)"
      } :+ cq"""_ => throw new CodecConfigurationException("Unexpected class type: " + $subClassName)"""
      q"$subClassName match { case ..$cases }"
    }

    def getListValue(key: c.universe.Literal, iterableType: c.universe.Type) = {
      require(isIterable(iterableType))
      q"document.get($key).asInstanceOf[java.util.List[_]]"
    }

    def getInstanceMethod(t: Type, fName: String): Tree = {
      val getOps = getFields(t).map({
        case (f, name) =>
          val key = keyName(name)
          val getterMethodName = getInstanceMethodName(fieldName(fName, f.typeSymbol.name.toTermName))
          val doc = q"document.get($key).asInstanceOf[org.bson.Document]"
          val getter = f match {
            case adt if isSealed(adt) => getInstanceOfSealedClass(adt, doc)
            case cc if isCaseClass(cc) => q"$getterMethodName($doc)"
            case seq if isIterableCaseClass(seq) =>
              val (cls, depth) = getIterableCaseClass(seq)
              val expandedMethodName = fieldName(fName, cls.typeSymbol.name.toTermName)
              q"""nestedJavaToScala(${getListValue(key, seq)}, $depth, ${getInstanceMethodName(expandedMethodName)}).asInstanceOf[$seq]"""
            case seq if isIterableSealedClass(seq) =>
              val (cls, depth) = getIterableSealedClass(seq)
              val expandedMethodName = fieldName(fName, cls.typeSymbol.name.toTermName)
              q"""nestedJavaToScala(${getListValue(key, seq)}, $depth, ${getInstanceMethodName(expandedMethodName)}).asInstanceOf[$seq]"""
            case seq if isIterable(seq) =>
              q"""toScala(${getListValue(key, seq)}).asInstanceOf[$seq]"""
            case opt if isOptionCaseClass(opt) =>
              val expandedMethodName = fieldName(fName, getOptionCaseClass(opt).typeSymbol.name.toTermName)
              q"toOption($doc, ${getInstanceMethodName(expandedMethodName)})"
            case optional if isOption(optional) => q"Option(document.get($key)).asInstanceOf[$f]"
            case v => q"document.get($key).asInstanceOf[$v]"
          }
          q"$name = $getter"
      })

      val getInstance = if (isSealed(t)) getInstanceOfSealedClass(t, q"document") else q"new ${t.resultType}(..$getOps)"
      q"""
         def ${getInstanceMethodName(fName)}(document: org.bson.Document): $t = {
            $getInstance
         }
       """
    }
    val getInstanceMethods = for (c <- allClasses) yield getInstanceMethod(c._1, c._2)

    c.Expr[Codec[T]](
      q"""
            import org.mongodb.scala.bson.codecs.MacroDocumentCodecBase
            new MacroDocumentCodecBase[$classTypeName] {
              import org.mongodb.scala.bson.codecs.MacroHelpers._
              import org.bson.codecs.configuration.CodecConfigurationException
              import org.bson.codecs.configuration.CodecRegistries._
              val codecRegistry = fromRegistries(fromCodecs(this), $codecRegistry)

              ..$getDocumentMethods
              ..$getInstanceMethods

              def getInstance(document: org.bson.Document): $classTypeName = {
                ${getInstanceMethodName(className)}(document)
              }

              def getDocument(instance: $classTypeName): org.bson.Document = {
                ${getDocumentMethodName(className)}(instance)
              }
              override def getEncoderClass: Class[$classTypeName] = classOf[$classTypeName]
             }
       """
    )
  }
  // scalastyle:on method.length cyclomatic.complexity

}
