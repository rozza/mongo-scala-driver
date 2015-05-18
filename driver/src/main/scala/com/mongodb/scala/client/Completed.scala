package com.mongodb.scala.client

/**
 * Representing a successful completed operation.
 *
 * @since 1.0
 */
object Completed {
  def apply() = new Completed(true);
}

case class Completed(completed: Boolean) {
}
