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

import org.bson.codecs.Codec
import org.bson.codecs.configuration.CodecRegistry

/**
 * Macro based Codecs
 *
 * @since 1.2
 */
object Macros {

  /**
   * Creates a Codec for a Case Class
   *
   * @tparam T the Case Class to create a Codec from
   * @return the Codec for the Case Class
   */
  @compileTimeOnly("Creating a codec utilises Macros and must be run at compile time.")
  def createCodec[T](): Codec[T] = macro MacroImpl.createCodecImplNoArgs[T]

  /**
   * Creates a Codec for a Case Class
   *
   * @param codecRegistry the Codec Registry to use
   * @tparam T the Case Class to create a codec from
   * @return the Codec for the Case Class
   */
  @compileTimeOnly("Creating a codec utilises Macros and must be run at compile time.")
  def createCodec[T](codecRegistry: CodecRegistry): Codec[T] = macro MacroImpl.createCodecImpl[T]

}
