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

object MacroMapImpl {
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
    def isIterableCaseClass(t: Type): Boolean = isIterable(t) && isCaseClass(t.typeArgs.head)
    def isIterableSealedClass(t: Type): Boolean = isIterable(t) && isSealed(t.typeArgs.head)
    def isMap(t: Type): Boolean = t.baseClasses.contains(mapType)
    def isOption(t: Type): Boolean = t.baseClasses.contains(optionType)
    def isOptionCaseClass(t: Type): Boolean = isOption(t) && isCaseClass(t.typeArgs.head)
    def isSealed(t: Type): Boolean = t.typeSymbol.isClass && t.typeSymbol.asClass.isSealed && getSubClasses(t).nonEmpty && !isOption(t)

    // Type Fetchers
    def getIterableCaseClass(t: Type): Type = t.typeArgs.head
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
                  val sealedClass = seq.typeArgs.head
                  val childCaseClasses = getSubClasses(sealedClass)
                  (sealedClass, fieldName(n, sealedClass.typeSymbol.name.toTermName)) :: childCaseClasses.flatMap(cls => {
                    val expandedIterableName = fieldName(n, cls.typeSymbol.name.toTermName)
                    if (seen.contains(cls)) {
                      List.empty[(Type, String)]
                    } else {
                      (cls, expandedIterableName) :: seq.typeArgs.flatMap(i => findCaseClasses(i, expandedIterableName, seen :+ cls))
                    }
                  })
                case seq if isIterableCaseClass(seq) =>
                  val cls = seq.typeArgs.head
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

    def getInstanceOfSealedClass(t: Type, map: Tree): Tree = {
      val subClassName = q"$map.get($classFieldName)"
      val cases: List[c.universe.Tree] = getSubClasses(t).map { st =>
        val methodName = fieldName("", st.typeSymbol.name.toTermName)
        cq"Some($methodName) => ${getInstanceMethodName(methodName)}($map)"
      } ++ List(
        cq"""Some(_) => throw new CodecConfigurationException("Unexpected class type: " + $subClassName)""",
        cq"""None => throw new CodecConfigurationException("Sealed Class expected but missing " + $classFieldName + " field data")"""
      )
      q"$subClassName match { case ..$cases }"
    }
    def getInstanceMethodName(fieldName: String): TermName = getMethodName("Ins")(fieldName)
    def getInstanceMethod(t: Type, fName: String): Tree = {
      val getOps = getFields(t).map({
        case (f, name) =>
          val key = keyName(name)
          val getterMethodName = getInstanceMethodName(fieldName(fName, f.typeSymbol.name.toTermName))
          val getter = f match {
            case adt if isSealed(adt) => getInstanceOfSealedClass(adt, q"map.get($key).get")
            case cc if isCaseClass(cc) => q"$getterMethodName(map.get($key).get)"
            case seq if isIterableCaseClass(seq) || isIterableSealedClass(seq) =>
              val cls = seq.typeArgs.head
              val expandedMethodName = fieldName(fName, cls.typeSymbol.name.toTermName)
              q"""map.get($key).map(i => ${getInstanceMethodName(expandedMethodName)}(i)).get"""
            case opt if isOptionCaseClass(opt) =>
              val expandedMethodName = fieldName(fName, getOptionCaseClass(opt).typeSymbol.name.toTermName)
              q"""map.get($key).map(i => ${getInstanceMethodName(expandedMethodName)}(i)).get"""
            case optional if isOption(optional) => q"map.get($key)"
            case v => q"map.get($key).get"
          }
          q"$name = $getter.asInstanceOf[$f]"
      })

      val getInstance = if (isSealed(t)) getInstanceOfSealedClass(t, q"map") else q"new ${t.resultType}(..$getOps)"
      q"""
         def ${getInstanceMethodName(fName)}(map: Map[String, _]): $t = {
            $getInstance
         }
       """
    }
    val getInstanceMethods = for (c <- allClasses) yield getInstanceMethod(c._1, c._2)

    def getMap: Tree = {
      val fieldPutOps = getFields(mainType).map({
        case (f, name) =>
          val key = keyName(name)
          val value = q"instance.$name"
          q"map.put($key, $value)"
      })
      q"""
         val map = mutable.Map[String, Any]()
          ..$fieldPutOps
          map.toMap
       """
    }

    c.Expr[Codec[T]](
      q"""
            import org.mongodb.scala.bson.codecs.MacroMapCodecBase
            new MacroMapCodecBase[$classTypeName] {
              import org.bson.codecs.configuration.CodecConfigurationException
              import org.bson.codecs.configuration.CodecRegistries._
              import scala.collection.mutable

              val codecRegistry = fromRegistries(fromCodecs(this), $codecRegistry)

              ..$getInstanceMethods

              def getInstance(map: Map[String, _ <: Any]): $classTypeName = {
                ${getInstanceMethodName(className)}(map)
              }

              def getMap(instance: $classTypeName): Map[String, _ <: Any] = {
                $getMap
              }

              override def getEncoderClass: Class[$classTypeName] = classOf[$classTypeName]
            }
       """
    )
  }
}
