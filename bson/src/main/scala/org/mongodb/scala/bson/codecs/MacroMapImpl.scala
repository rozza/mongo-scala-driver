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

    // Names
    val classTypeName = mainType.typeSymbol.name.toTypeName

    def keyName(t: TermName): Literal = Literal(Constant(t.toString))
    def getFields(t: Type): List[(Type, TermName)] = {
      t.members.sorted.filter(_.isMethod).map(_.asMethod).filter(_.isGetter).map(m => (m.returnType.asSeenFrom(t, t.typeSymbol), m.name))
    }

    def getInstance: Tree = {
      val getOps = getFields(mainType).map({
        case (f, name) =>
          val key = keyName(name)
          val getter = q"map.get($key).get.asInstanceOf[$f]"
          q"$name = $getter"
      })
      q"new ${mainType.resultType}(..$getOps)"
    }

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

              def getInstance(map: Map[String, _ <: Any]): $classTypeName = {
                $getInstance
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
