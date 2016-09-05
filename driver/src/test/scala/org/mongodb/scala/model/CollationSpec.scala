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

package org.mongodb.scala.model

import java.lang.reflect.Modifier.isStatic

import org.scalatest.{ FlatSpec, Matchers }

class CollationSpec extends FlatSpec with Matchers {

  "Collation" should "have the same static fields as the wrapped Collation" in {
    val collationClass: Class[Collation] = classOf[com.mongodb.client.model.Collation]
    val wrappedFields = collationClass.getDeclaredFields.filter(f => isStatic(f.getModifiers)).map(_.getName).toSet
    val wrappedMethods = collationClass.getDeclaredMethods.filter(f => isStatic(f.getModifiers)).map(_.getName).toSet
    val exclusions = Set("$VALUES", "valueOf", "values")

    val wrapped = (wrappedFields ++ wrappedMethods) -- exclusions
    val local = Collation.getClass.getDeclaredMethods.map(_.getName).toSet -- Set("apply")

    local should equal(wrapped)
  }

  it should "return the underlying builder" in {
    Collation.builder().getClass should equal(classOf[com.mongodb.client.model.Collation.Builder])
  }

}
