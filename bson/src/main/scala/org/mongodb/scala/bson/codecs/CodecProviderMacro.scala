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

object CodecProviderMacro {

  // scalastyle:off method.length
  def createCodecProvider[T: c.WeakTypeTag](c: whitebox.Context)(): c.Expr[CodecProvider] = {
    import c.universe._

    // Declared types
    val mainType = weakTypeOf[T]

    // Names
    val classTypeName = mainType.typeSymbol.name.toTypeName

    def exprCodecRegistry = c.Expr[CodecRegistry](q"codecRegistry")
    def codec = CodecMacro.createCodec[T](c)(exprCodecRegistry)

    c.Expr[CodecProvider](
      q"""
         import org.mongodb.scala.bson.codecs.CodecMacro
         import org.bson.codecs.Codec
         import org.bson.codecs.configuration.{ CodecProvider, CodecRegistry }

         new CodecProvider {
           @SuppressWarnings(Array("unchecked"))
           def get[C](clazz: Class[C], codecRegistry: CodecRegistry): Codec[C] = {
              $codec
           }
         }
       """
    )
  }
  // scalastyle:on method.length
}
