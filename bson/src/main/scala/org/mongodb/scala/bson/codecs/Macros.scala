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

import org.bson.codecs.Codec
import org.bson.codecs.configuration.{ CodecProvider, CodecRegistry }

import org.mongodb.scala.bson.codecs.macrocodecs.{ CaseClassCodec, CaseClassProvider, SealedCaseClassCodec }

/**
 * Macro based Codecs
 *
 * @since 2.0
 */
object Macros {

  /**
   * Creates a Codec for a Case Class
   *
   * @tparam T the Case Class to create a Codec from
   * @return the Codec for the Case Class
   */
  @compileTimeOnly("Creating a Codec utilises Macros and must be run at compile time.")
  def createCodec[T](): Codec[T] = macro CaseClassCodec.createCodecNoArgs[T]

  @compileTimeOnly("Creating a Codec utilises Macros and must be run at compile time.")
  def createSealedCodec[T](): Codec[T] = macro SealedCaseClassCodec.createCodecNoArgs[T]

  /**
   * Creates a Codec for a Case Class
   *
   * @param codecRegistry the Codec Registry to use
   * @tparam T the Case Class to create a codec from
   * @return the Codec for the Case Class
   */
  @compileTimeOnly("Creating a Codec utilises Macros and must be run at compile time.")
  def createCodec[T](codecRegistry: CodecRegistry): Codec[T] = macro CaseClassCodec.createCodec[T]

  /**
   * Creates a CodecProvider for a Case Class
   *
   * @tparam T the Case Class to create a Codec from
   * @return the CodecProvider for the Case Class
   */
  @compileTimeOnly("Creating a CodecProvider utilises Macros and must be run at compile time.")
  def createCodecProvider[T](): CodecProvider = macro CaseClassProvider.createCaseClassProvider[T]

  /**
   * Creates a CodecProvider for a Case Class using the given class
   *
   * @param clazz the clazz that is the case class
   * @tparam T the Case Class to create a Codec from
   * @return the CodecProvider for the Case Class
   */
  @compileTimeOnly("Creating a CodecProvider utilises Macros and must be run at compile time.")
  implicit def createCodecProvider[T](clazz: Class[T]): CodecProvider = macro CaseClassProvider.createCaseClassProviderWithClass[T]

}
