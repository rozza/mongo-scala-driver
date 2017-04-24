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

import scala.annotation.compileTimeOnly
import scala.language.experimental.macros
import scala.language.implicitConversions
import scala.annotation.{ StaticAnnotation, meta }

import org.bson.codecs.Codec
import org.bson.codecs.configuration.{ CodecProvider, CodecRegistry }

import org.mongodb.scala.bson.codecs.macrocodecs.{ CaseClassCodec, CaseClassProvider }

/**
 * Macro based Codecs
 *
 * Allows the compile time creation of Codecs for case classes.
 *
 * The recommended approach is to use the implicit [[Macros.createCodecProvider]] method to help build a codecRegistry:
 * ```
 * import org.mongodb.scala.bson.codecs.Macros.createCodecProvider
 * import org.bson.codecs.configuration.CodecRegistries.{fromRegistries, fromProviders}
 *
 * case class Contact(phone: String)
 * case class User(_id: Int, username: String, age: Int, hobbies: List[String], contacts: List[Contact])
 *
 * val codecRegistry = fromRegistries(fromProviders(classOf[User], classOf[Contact]), MongoClient.DEFAULT_CODEC_REGISTRY)
 * ```
 *
 * @since 2.0
 */
object Macros {

  /**
   * Creates a CodecProvider for a case class
   *
   * @tparam T the case class to create a Codec from
   * @return the CodecProvider for the case class
   */
  @compileTimeOnly("Creating a CodecProvider utilises Macros and must be run at compile time.")
  def createCodecProvider[T](): CodecProvider = macro CaseClassProvider.createCodecProviderNoIgnoreNone[T]

  /**
   * Creates a CodecProvider for a case class using the given class to represent the case class
   *
   * @param clazz the clazz that is the case class
   * @tparam T the case class to create a Codec from
   * @return the CodecProvider for the case class
   */
  @compileTimeOnly("Creating a CodecProvider utilises Macros and must be run at compile time.")
  implicit def createCodecProvider[T](clazz: Class[T]): CodecProvider = macro CaseClassProvider.createCodecProviderWithClassNoIgnoreNone[T]

  /**
   * Creates a CodecProvider for a case class that ignores any `None` values
   *
   * @tparam T the case class to create a Codec from
   * @return the CodecProvider for the case class
   */
  @compileTimeOnly("Creating a CodecProvider utilises Macros and must be run at compile time.")
  def createCodecProviderIgnoreNone[T](): CodecProvider = macro CaseClassProvider.createCodecProviderIgnoreNone[T]

  /**
   * Creates a CodecProvider for a case class that ignores any `None` values, using the given class to represent the case class
   *
   * @param clazz the clazz that is the case class
   * @tparam T the case class to create a Codec from
   * @return the CodecProvider for the case class
   */
  @compileTimeOnly("Creating a CodecProvider utilises Macros and must be run at compile time.")
  def createCodecProviderIgnoreNone[T](clazz: Class[T]): CodecProvider = macro CaseClassProvider.createCodecProviderWithClassIgnoreNone[T]

  /**
   * Creates a Codec for a case class
   *
   * @tparam T the case class to create a Codec from
   * @return the Codec for the case class
   */
  @compileTimeOnly("Creating a Codec utilises Macros and must be run at compile time.")
  def createCodec[T](): Codec[T] = macro CaseClassCodec.createCodecNoCodecRegistryNoIgnoreNone[T]

  /**
   * Creates a Codec for a case class
   *
   * @tparam T the case class to create a Codec from
   * @return the Codec for the case class
   */
  @compileTimeOnly("Creating a Codec utilises Macros and must be run at compile time.")
  def createCodec[T](ignoreNone: Boolean): Codec[T] = macro CaseClassCodec.createCodecNoCodecRegistry[T]

  /**
   * Creates a Codec for a case class
   *
   * @param codecRegistry the Codec Registry to use
   * @tparam T the case class to create a codec from
   * @return the Codec for the case class
   */
  @compileTimeOnly("Creating a Codec utilises Macros and must be run at compile time.")
  def createCodec[T](codecRegistry: CodecRegistry): Codec[T] = macro CaseClassCodec.createCodecIgnoreNone[T]

  /**
   * Annotations to use on case classes that are being processed by macros.
   *
   * @since 2.1
   */
  object Annotations {

    /**
     * Ignores any None values in the case class
     *
     * {{{
     * @IgnoreNone
     * case class Person(name: String, nickName: Option[String])
     *
     * Person(name = "John", nickName: None) // becomes the equivalent of: { name: "John" }
     * }}}
     */
    @meta.param
    case class IgnoreNone() extends StaticAnnotation

    /**
     * Specify a different key when converting the field into Bson.
     *
     * {{{
     * case class User(@Key("_id") id: String)
     * }}}
     *
     * @param key the field name to use when storing or reading the data from Bson.
     */
    @meta.param
    case class Key(key: String) extends StaticAnnotation
  }

}
