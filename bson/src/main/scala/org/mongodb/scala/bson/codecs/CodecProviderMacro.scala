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

import org.bson.codecs.configuration.{ CodecProvider, CodecRegistry }

private[bson] object CodecProviderMacro {

  def createCodecProviderWithClass[T: c.WeakTypeTag](c: whitebox.Context)(clazz: c.Expr[Class[T]]): c.Expr[CodecProvider] =
    createCodecProvider[T](c)().asInstanceOf[c.Expr[CodecProvider]]

  def createCodecProvider[T: c.WeakTypeTag](c: whitebox.Context)(): c.Expr[CodecProvider] = {
    import c.universe._

    // Declared type
    val mainType = weakTypeOf[T]

    // Helpers
    def isOption(t: Type): Boolean = t.typeSymbol == definitions.OptionClass
    def isSealed(t: Type): Boolean = !isOption(t) && t.typeSymbol.isClass && t.typeSymbol.asClass.isSealed

    // Names
    def exprCodecRegistry = c.Expr[CodecRegistry](q"codecRegistry")
    def codec = {
      if (isSealed(mainType)) {
        CodecMacro.createCodec[T](c)(exprCodecRegistry)
      } else {
        CodecMacro.createCodec[T](c)(exprCodecRegistry)
      }
    }

    c.Expr[CodecProvider](
      q"""
         import org.mongodb.scala.bson.codecs.CodecMacro
         import org.bson.codecs.Codec
         import org.bson.codecs.configuration.{ CodecProvider, CodecRegistry }

         new CodecProvider {
           @SuppressWarnings(Array("unchecked"))
           def get[C](clazz: Class[C], codecRegistry: CodecRegistry): Codec[C] = {
              if (classOf[$mainType].isAssignableFrom(clazz)) {
                $codec.asInstanceOf[Codec[C]]
              } else {
                null
              }
           }
         }
       """
    )
  }
}
