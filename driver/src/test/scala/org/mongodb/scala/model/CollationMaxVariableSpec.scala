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

import java.lang.reflect.Modifier._

import scala.util.{ Success, Try }

import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.{ FlatSpec, Matchers }

class CollationMaxVariableSpec extends FlatSpec with Matchers {

  "CollationMaxVariable" should "have the same static fields as the wrapped CollationMaxVariable" in {
    val collationMaxVariableClass: Class[CollationMaxVariable] = classOf[com.mongodb.client.model.CollationMaxVariable]
    val wrappedFields = collationMaxVariableClass.getDeclaredFields.filter(f => isStatic(f.getModifiers)).map(_.getName).toSet
    val wrappedMethods = collationMaxVariableClass.getDeclaredMethods.filter(f => isStatic(f.getModifiers)).map(_.getName).toSet
    val exclusions = Set("$VALUES", "valueOf", "values")

    val wrapped = (wrappedFields ++ wrappedMethods) -- exclusions
    val local = CollationMaxVariable.getClass.getDeclaredMethods.map(_.getName).toSet -- Set("apply")

    local should equal(wrapped)
  }

  it should "return the expected CollationMaxVariables" in {
    forAll(collationMaxVariableActions) { (stringValue: String, expectedValue: Try[CollationMaxVariable]) =>
      CollationMaxVariable.fromString(stringValue) should equal(expectedValue)
    }
  }

  it should "handle invalid strings" in {
    forAll(invalidCollationMaxVariables) { (stringValue: String) =>
      CollationMaxVariable.fromString(stringValue) should be a 'failure
    }
  }

  val collationMaxVariableActions =
    Table(
      ("stringValue", "JavaValue"),
      ("punct", Success(CollationMaxVariable.PUNCT)),
      ("space", Success(CollationMaxVariable.SPACE))
    )

  val invalidCollationMaxVariables = Table("invalid strings", "SPACE", "PUNCT")
}
