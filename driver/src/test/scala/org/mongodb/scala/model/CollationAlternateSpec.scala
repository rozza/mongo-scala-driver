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

class CollationAlternateSpec extends FlatSpec with Matchers {

  "CollationAlternate" should "have the same static fields as the wrapped CollationAlternative" in {
    val collationAlternativeClass: Class[CollationAlternate] = classOf[com.mongodb.client.model.CollationAlternate]
    val wrappedFields = collationAlternativeClass.getDeclaredFields.filter(f => isStatic(f.getModifiers)).map(_.getName).toSet
    val wrappedMethods = collationAlternativeClass.getDeclaredMethods.filter(f => isStatic(f.getModifiers)).map(_.getName).toSet
    val exclusions = Set("$VALUES", "valueOf", "values")

    val wrapped = (wrappedFields ++ wrappedMethods) -- exclusions
    val local = CollationAlternate.getClass.getDeclaredMethods.map(_.getName).toSet -- Set("apply")

    local should equal(wrapped)
  }

  it should "return the expected CollationAlternatives" in {
    forAll(collationAlternativeActions) { (stringValue: String, expectedValue: Try[CollationAlternate]) =>
      CollationAlternate.fromString(stringValue) should equal(expectedValue)
    }
  }

  it should "handle invalid strings" in {
    forAll(invalidCollationAlternatives) { (stringValue: String) =>
      CollationAlternate.fromString(stringValue) should be a 'failure
    }
  }

  val collationAlternativeActions =
    Table(
      ("stringValue", "JavaValue"),
      ("non-ignorable", Success(CollationAlternate.NON_IGNORABLE)),
      ("shifted", Success(CollationAlternate.SHIFTED))
    )

  val invalidCollationAlternatives = Table("invalid strings", "NON_IGNORABLE", "SHIFTED")
}
