package org.mongodb.scala.core.collection

import org.bson.BsonValue

import scala.language.implicitConversions

/**
 * Maps are required to a plus operation which takes `Map[A, B]` to a `Map[A, +B]` which
 * isn't supported with Documents which essentially are typesafe `Maps[A, BsonValue]`.
 *
 * Defining the operation in a separate trait helps IDE's disambiguate between
 * `+[B](kv, (String, B))(implicit ev: B => BsonValue)` and `+[B1 >: BsonValue](kv: (String, B1))`
 */
trait DocumentLowPriorityOperations {

  /**
   * For Maps this would add a key/value pair to this map, returning a new map
   *
   * As we have a fixed value type of `BsonValue` this is not allowed so if you can't implicitly
   * convert to `BsonValue` then this will fail
   *
   * @param    kv the key/value pair
   *  @return   A thrown NotImplementedError exception
   */
  def +[B1 >: BsonValue](kv: (String, B1)) =
    throw new NotImplementedError(s"You cannot add a non BsonValue type to a Document : $kv")

}
