/*
 * Copyright 2015 MongoDB, Inc.
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

package org.mongodb.scala.model

import java.lang

import scala.collection.JavaConverters._
import scala.util.matching.Regex

import org.bson._
import org.bson.conversions.Bson
import com.mongodb.client.model.geojson.{ Geometry, Point }
import com.mongodb.client.model.{ Filters => JFilters }

//scalastyle:off null number.of.methods
/**
 * A factory for query filters. A convenient way to use this class is to statically import all of its methods, which allows usage like:
 *
 * `collection.find(and(eq("x", 1), lt("y", 3)))`
 *
 * @since 1.0
 */
object Filters {
  /**
   * Creates a filter that matches all documents where the value of the field name equals the specified value. Note that this does
   * actually generate a `\$eq` operator, as the query language doesn't require it.
   *
   * @param fieldName the field name
   * @param value     the value
   * @tparam TItem  the value type
   * @return the filter
   * @see [[http://docs.mongodb.org/manual/reference/operator/query/eq \$eq]]
   */
  def eq[TItem](fieldName: String, value: TItem): Bson = JFilters.eq(fieldName, value)

  /**
   * Creates a filter that matches all documents where the value of the field name does not equal the specified value.
   *
   * @param fieldName the field name
   * @param value     the value
   * @tparam TItem  the value type
   * @return the filter
   * @see [[http://docs.mongodb.org/manual/reference/operator/query/ne \$ne]]
   */
  def ne[TItem](fieldName: String, value: TItem): Bson = JFilters.ne(fieldName, value)

  /**
   * Creates a filter that matches all documents where the value of the given field is greater than the specified value.
   *
   * @param fieldName the field name
   * @param value the value
   * @tparam TItem the value type
   * @return the filter
   * @see [[http://docs.mongodb.org/manual/reference/operator/query/gt \$gt]]
   */
  def gt[TItem](fieldName: String, value: TItem): Bson = JFilters.gt(fieldName, value)

  /**
   * Creates a filter that matches all documents where the value of the given field is less than the specified value.
   *
   * @param fieldName the field name
   * @param value the value
   * @tparam TItem the value type
   * @return the filter
   * @see [[http://docs.mongodb.org/manual/reference/operator/query/lt \$lt]]
   */
  def lt[TItem](fieldName: String, value: TItem): Bson = JFilters.lt(fieldName, value)

  /**
   * Creates a filter that matches all documents where the value of the given field is greater than or equal to the specified value.
   *
   * @param fieldName the field name
   * @param value the value
   * @tparam TItem the value type
   * @return the filter
   * @see [[http://docs.mongodb.org/manual/reference/operator/query/gte \$gte]]
   */
  def gte[TItem](fieldName: String, value: TItem): Bson = JFilters.gte(fieldName, value)

  /**
   * Creates a filter that matches all documents where the value of the given field is less than or equal to the specified value.
   *
   * @param fieldName the field name
   * @param value the value
   * @tparam TItem the value type
   * @return the filter
   * @see [[http://docs.mongodb.org/manual/reference/operator/query/lte \$lte]]
   */
  def lte[TItem](fieldName: String, value: TItem): Bson = JFilters.lte(fieldName: String, value: TItem)

  /**
   * Creates a filter that matches all documents where the value of a field equals any value in the list of specified values.
   *
   * @param fieldName the field name
   * @param values    the list of values
   * @tparam TItem   the value type
   * @return the filter
   * @see [[http://docs.mongodb.org/manual/reference/operator/query/in \$in]]
   */
  def in[TItem](fieldName: String, values: TItem*): Bson = JFilters.in(fieldName, values.asJava)

  /**
   * Creates a filter that matches all documents where the value of a field equals any value in the list of specified values.
   *
   * @param fieldName the field name
   * @param values    the list of values
   * @tparam TItem   the value type
   * @return the filter
   * @see [[http://docs.mongodb.org/manual/reference/operator/query/in \$in]]
   */
  def in[TItem](fieldName: String, values: List[TItem]): Bson = JFilters.in(fieldName, values.asJava)

  /**
   * Creates a filter that matches all documents where the value of a field does not equal any of the specified values or does not exist.
   *
   * @param fieldName the field name
   * @param values    the list of values
   * @tparam TItem   the value type
   * @return the filter
   * @see [[http://docs.mongodb.org/manual/reference/operator/query/nin \$nin]]
   */
  def nin[TItem](fieldName: String, values: TItem*): Bson = JFilters.nin(fieldName, values.asJava)

  /**
   * Creates a filter that matches all documents where the value of a field does not equal any of the specified values or does not exist.
   *
   * @param fieldName the field name
   * @param values    the list of values
   * @tparam TItem   the value type
   * @return the filter
   * @see [[http://docs.mongodb.org/manual/reference/operator/query/nin \$nin]]
   */
  def nin[TItem](fieldName: String, values: List[TItem]): Bson = JFilters.nin(fieldName, values.asJava)

  /**
   * Creates a filter that performs a logical AND of the provided list of filters.  Note that this will only generate a `\$and`
   * operator if absolutely necessary, as the query language implicity ands together all the keys.  In other words, a query expression
   * like:
   *
   * `and(eq("x", 1), lt("y", 3))`
   *
   * will generate a MongoDB query like:
   * `{x : 1, y : {\$lt : 3}}`
   *
   * @param filters the list of filters to and together
   * @return the filter
   * @see [[http://docs.mongodb.org/manual/reference/operator/query/and \$and]]
   */
  def and(filters: List[Bson]): Bson = JFilters.and(filters.asJava)

  /**
   * Creates a filter that performs a logical AND of the provided list of filters.  Note that this will only generate a "\$and"
   * operator if absolutely necessary, as the query language implicity ands together all the keys.  In other words, a query expression
   * like:
   *
   * <blockquote><pre>
   * and(eq("x", 1), lt("y", 3))
   * </pre></blockquote>
   *
   * will generate a MongoDB query like:
   *
   * <blockquote><pre>
   * {x : 1, y : {\$lt : 3}}
   * </pre></blockquote>
   *
   * @param filters the list of filters to and together
   * @return the filter
   * @see [[http://docs.mongodb.org/manual/reference/operator/query/and \$and]]
   */
  def and(filters: Bson*): Bson = JFilters.and(filters.asJava)

  /**
   * Creates a filter that preforms a logical OR of the provided list of filters.
   *
   * @param filters the list of filters to and together
   * @return the filter
   * @see [[http://docs.mongodb.org/manual/reference/operator/query/or \$or]]
   */
  def or(filters: Iterable[Bson]): Bson = JFilters.or(filters.asJava)

  /**
   * Creates a filter that preforms a logical OR of the provided list of filters.
   *
   * @param filters the list of filters to and together
   * @return the filter
   * @see [[http://docs.mongodb.org/manual/reference/operator/query/or \$or]]
   */
  def or(filters: Bson*): Bson = JFilters.or(filters.asJava)

  /**
   * Creates a filter that matches all documents that do not match the passed in filter.
   * Requires the field name to passed as part of the value passed in and lifts it to create a valid "\$not" query:
   *
   * `not(eq("x", 1))`
   *
   * will generate a MongoDB query like:
   * `{x :\$not: {\$eq : 1}}`
   *
   * @param filter     the value
   * @return the filter
   * @see [[http://docs.mongodb.org/manual/reference/operator/query/not \$not]]
   */
  def not(filter: Bson): Bson = JFilters.not(filter)

  /**
   * Creates a filter that performs a logical NOR operation on all the specified filters.
   *
   * @param filters    the list of values
   * @return the filter
   * @see [[http://docs.mongodb.org/manual/reference/operator/query/nor \$nor]]
   */
  def nor(filters: Bson*): Bson = JFilters.nor(filters.asJava)

  /**
   * Creates a filter that performs a logical NOR operation on all the specified filters.
   *
   * @param filters    the list of values
   * @return the filter
   * @see [[http://docs.mongodb.org/manual/reference/operator/query/nor \$nor]]
   */
  def nor(filters: Iterable[Bson]): Bson = JFilters.nor(filters.asJava)

  /**
   * Creates a filter that matches all documents that contain the given field.
   *
   * @param fieldName the field name
   * @return the filter
   * @see [[http://docs.mongodb.org/manual/reference/operator/query/exists \$exists]]
   */
  def exists(fieldName: String): Bson = JFilters.exists(fieldName)

  /**
   * Creates a filter that matches all documents that either contain or do not contain the given field, depending on the value of the
   * exists parameter.
   *
   * @param fieldName the field name
   * @param exists    true to check for existence, false to check for absence
   * @return the filter
   * @see [[http://docs.mongodb.org/manual/reference/operator/query/exists \$exists]]
   */
  def exists(fieldName: String, exists: Boolean): Bson = JFilters.exists(fieldName, exists)

  /**
   * Creates a filter that matches all documents where the value of the field is of the specified BSON type.
   *
   * @param fieldName the field name
   * @param type      the BSON type
   * @return the filter
   * @see [[http://docs.mongodb.org/manual/reference/operator/query/type \$type]]
   */
  def `type`(fieldName: String, `type`: BsonType): Bson = JFilters.`type`(fieldName, `type`) //scalastyle:ignore

  /**
   * Creates a filter that matches all documents where the value of a field divided by a divisor has the specified remainder (i.e. perform
   * a modulo operation to select documents).
   *
   * @param fieldName the field name
   * @param divisor   the modulus
   * @param remainder the remainder
   * @return the filter
   * @see [[http://docs.mongodb.org/manual/reference/operator/query/mod \$mod]]
   */
  def mod(fieldName: String, divisor: Long, remainder: Long): Bson = JFilters.mod(fieldName, divisor, remainder)

  /**
   * Creates a filter that matches all documents where the value of the field matches the given regular expression pattern with the given
   * options applied.
   *
   * @param fieldName the field name
   * @param pattern   the pattern
   * @return the filter
   * @see [[http://docs.mongodb.org/manual/reference/operator/query/regex \$regex]]
   */
  def regex(fieldName: String, pattern: String): Bson = JFilters.regex(fieldName, pattern)

  /**
   * Creates a filter that matches all documents where the value of the field matches the given regular expression pattern with the given
   * options applied.
   *
   * @param fieldName the field name
   * @param pattern   the pattern
   * @param options   the options
   * @return the filter
   * @see [[http://docs.mongodb.org/manual/reference/operator/query/regex \$regex]]
   */
  def regex(fieldName: String, pattern: String, options: String): Bson = JFilters.regex(fieldName: String, pattern: String, options: String)

  private val date: Regex = """(\d\d\d\d)-(\d\d)-(\d\d)""".r

  /**
   * Creates a filter that matches all documents where the value of the field matches the given regular expression pattern with the given
   * options applied.
   *
   * @param fieldName the field name
   * @param regex   the regex
   * @return the filter
   * @see [[http://docs.mongodb.org/manual/reference/operator/query/regex \$regex]]
   * @since 1.0
   */
  def regex(fieldName: String, regex: Regex): Bson = JFilters.regex(fieldName, regex.pattern)

  /**
   * Creates a filter that matches all documents matching the given search term.
   *
   * @param search the search term
   * @return the filter
   * @see [[http://docs.mongodb.org/manual/reference/operator/query/text \$text]]
   */
  def text(search: String): Bson = JFilters.text(search)

  /**
   * Creates a filter that matches all documents matching the given search term using the given language.
   *
   * @param search   the search term
   * @param language the language to use for stop words
   * @return the filter
   * @see [[http://docs.mongodb.org/manual/reference/operator/query/text \$text]]
   */
  def text(search: String, language: String): Bson = JFilters.text(search, language)

  /**
   * Creates a filter that matches all documents for which the given expression is true.
   *
   * @param javaScriptExpression the JavaScript expression
   * @return the filter
   * @see [[http://docs.mongodb.org/manual/reference/operator/query/where \$where]]
   */
  def where(javaScriptExpression: String): Bson = JFilters.where(javaScriptExpression)

  /**
   * Creates a filter that matches all documents where the value of a field is an array that contains all the specified values.
   *
   * @param fieldName the field name
   * @param values    the list of values
   * @tparam TItem   the value type
   * @return the filter
   * @see [[http://docs.mongodb.org/manual/reference/operator/query/all \$all]]
   */
  def all[TItem](fieldName: String, values: TItem*): Bson = JFilters.all(fieldName, values.toList.asJava)

  /**
   * Creates a filter that matches all documents where the value of a field is an array that contains all the specified values.
   *
   * @param fieldName the field name
   * @param values    the list of values
   * @tparam TItem   the value type
   * @return the filter
   * @see [[http://docs.mongodb.org/manual/reference/operator/query/all \$all]]
   */
  def all[TItem](fieldName: String, values: List[TItem]): Bson = JFilters.all(fieldName, values.asJava)

  /**
   * Creates a filter that matches all documents containing a field that is an array where at least one member of the array matches the
   * given filter.
   *
   * @param fieldName the field name
   * @param filter    the filter to apply to each element
   * @return the filter
   * @see [[http://docs.mongodb.org/manual/reference/operator/query/elemMatch \$elemMatch]]
   */
  def elemMatch(fieldName: String, filter: Bson): Bson = JFilters.elemMatch(fieldName, filter)

  /**
   * Creates a filter that matches all documents where the value of a field is an array of the specified size.
   *
   * @param fieldName the field name
   * @param size      the size of the array
   * @return the filter
   * @see [[http://docs.mongodb.org/manual/reference/operator/query/size \$size]]
   */
  def size(fieldName: String, size: Int): Bson = JFilters.size(fieldName, size)

  /**
   * Creates a filter that matches all documents containing a field with geospatial data that exists entirely within the specified shape.
   *
   * @param fieldName the field name
   * @param geometry the bounding GeoJSON geometry object
   * @return the filter
   * @see [[http://docs.mongodb.org/manual/reference/operator/query/geoWithin/ \$geoWithin]]
   */
  def geoWithin(fieldName: String, geometry: Geometry): Bson = JFilters.geoWithin(fieldName, geometry)

  /**
   * Creates a filter that matches all documents containing a field with geospatial data that exists entirely within the specified shape.
   *
   * @param fieldName the field name
   * @param geometry the bounding GeoJSON geometry object
   * @return the filter
   * @see [[http://docs.mongodb.org/manual/reference/operator/query/geoWithin/ \$geoWithin]]
   */
  def geoWithin(fieldName: String, geometry: Bson): Bson = JFilters.geoWithin(fieldName, geometry)

  /**
   * Creates a filter that matches all documents containing a field with grid coordinates data that exist entirely within the specified
   * box.
   *
   * @param fieldName   the field name
   * @param lowerLeftX  the lower left x coordinate of the box
   * @param lowerLeftY  the lower left y coordinate of the box
   * @param upperRightX the upper left x coordinate of the box
   * @param upperRightY the upper left y coordinate of the box
   * @return the filter
   * @see [[http://docs.mongodb.org/manual/reference/operator/query/geoWithin/ \$geoWithin]]
   * @see [[http://docs.mongodb.org/manual/reference/operator/query/box/#op._S_box \$box]]
   */
  def geoWithinBox(fieldName: String, lowerLeftX: Double, lowerLeftY: Double, upperRightX: Double, upperRightY: Double): Bson =
    JFilters.geoWithinBox(fieldName, lowerLeftX, lowerLeftY, upperRightX, upperRightY)

  /**
   * Creates a filter that matches all documents containing a field with grid coordinates data that exist entirely within the specified
   * polygon.
   *
   * @param fieldName the field name
   * @param points    a list of pairs of x, y coordinates.  Any extra dimensions are ignored
   * @return the filter
   * @see [[http://docs.mongodb.org/manual/reference/operator/query/geoWithin/ \$geoWithin]]
   * @see [[http://docs.mongodb.org/manual/reference/operator/query/polygon/#op._S_polygon \$polygon]]
   */
  def geoWithinPolygon(fieldName: String, points: List[List[Double]]): Bson =
    JFilters.geoWithinPolygon(fieldName, points.map(_.asInstanceOf[List[lang.Double]].asJava).asJava)

  /**
   * Creates a filter that matches all documents containing a field with grid coordinates data that exist entirely within the specified
   * circle.
   *
   * @param fieldName the field name
   * @param x         the x coordinate of the circle
   * @param y         the y coordinate of the circle
   * @param radius    the radius of the circle, as measured in the units used by the coordinate system
   * @return the filter
   * @see [[http://docs.mongodb.org/manual/reference/operator/query/geoWithin/ \$geoWithin]]
   * @see [[http://docs.mongodb.org/manual/reference/operator/query/center/#op._S_center \$center]]
   */
  def geoWithinCenter(fieldName: String, x: Double, y: Double, radius: Double): Bson = JFilters.geoWithinCenter(fieldName, x, y, radius)

  /**
   * Creates a filter that matches all documents containing a field with geospatial data (GeoJSON or legacy coordinate pairs) that exist
   * entirely within the specified circle, using spherical geometry.  If using longitude and latitude, specify longitude first.
   *
   * @param fieldName the field name
   * @param x         the x coordinate of the circle
   * @param y         the y coordinate of the circle
   * @param radius    the radius of the circle, in radians
   * @return the filter
   * @see [[http://docs.mongodb.org/manual/reference/operator/query/geoWithin/ \$geoWithin]]
   * @see [[http://docs.mongodb.org/manual/reference/operator/query/centerSphere/#op._S_centerSphere \$centerSphere]]
   */
  def geoWithinCenterSphere(fieldName: String, x: Double, y: Double, radius: Double): Bson = JFilters.geoWithinCenterSphere(fieldName, x, y, radius)

  /**
   * Creates a filter that matches all documents containing a field with geospatial data that intersects with the specified shape.
   *
   * @param fieldName the field name
   * @param geometry the bounding GeoJSON geometry object
   * @return the filter
   * @see [[http://docs.mongodb.org/manual/reference/operator/query/geoIntersects/ \$geoIntersects]]
   */
  def geoIntersects(fieldName: String, geometry: Bson): Bson = JFilters.geoIntersects(fieldName, geometry)

  /**
   * Creates a filter that matches all documents containing a field with geospatial data that intersects with the specified shape.
   *
   * @param fieldName the field name
   * @param geometry the bounding GeoJSON geometry object
   * @return the filter
   * @see [[http://docs.mongodb.org/manual/reference/operator/query/geoIntersects/ \$geoIntersects]]
   */
  def geoIntersects(fieldName: String, geometry: Geometry): Bson = JFilters.geoIntersects(fieldName, geometry)

  /**
   * Creates a filter that matches all documents containing a field with geospatial data that is near the specified GeoJSON point.
   *
   * @param fieldName the field name
   * @param geometry the bounding GeoJSON geometry object
   * @return the filter
   * @see [[http://docs.mongodb.org/manual/reference/operator/query/near/ \$near]]
   */
  def near(fieldName: String, geometry: Point): Bson = JFilters.near(fieldName, geometry, null, null)

  /**
   * Creates a filter that matches all documents containing a field with geospatial data that is near the specified GeoJSON point.
   *
   * @param fieldName the field name
   * @param geometry the bounding GeoJSON geometry object
   * @param maxDistance the optional maximum distance from the point, in meters
   * @param minDistance the optional minimum distance from the point, in meters
   * @return the filter
   * @see [[http://docs.mongodb.org/manual/reference/operator/query/near/ \$near]]
   */
  def near(fieldName: String, geometry: Point, maxDistance: Option[Double], minDistance: Option[Double]): Bson = {
    (maxDistance, minDistance) match {
      case (Some(_), Some(_)) => JFilters.near(fieldName, geometry, maxDistance.get, minDistance.get)
      case (Some(_), None)    => JFilters.near(fieldName, geometry, maxDistance.get, null)
      case (None, Some(_))    => JFilters.near(fieldName, geometry, null, minDistance.get)
      case (None, None)       => JFilters.near(fieldName, geometry, null, null)
    }
  }

  /**
   * Creates a filter that matches all documents containing a field with geospatial data that is near the specified GeoJSON point.
   *
   * @param fieldName the field name
   * @param geometry the bounding GeoJSON geometry object
   * @return the filter
   * @see [[http://docs.mongodb.org/manual/reference/operator/query/near/ \$near]]
   */
  def near(fieldName: String, geometry: Bson): Bson = JFilters.near(fieldName, geometry, null, null)

  /**
   * Creates a filter that matches all documents containing a field with geospatial data that is near the specified GeoJSON point.
   *
   * @param fieldName the field name
   * @param geometry the bounding GeoJSON geometry object
   * @param maxDistance the optional maximum distance from the point, in meters
   * @param minDistance the optional minimum distance from the point, in meters
   * @return the filter
   * @see [[http://docs.mongodb.org/manual/reference/operator/query/near/ \$near]]
   */
  def near(fieldName: String, geometry: Bson, maxDistance: Option[Double], minDistance: Option[Double]): Bson = {
    (maxDistance, minDistance) match {
      case (Some(_), Some(_)) => JFilters.near(fieldName, geometry, maxDistance.get, minDistance.get)
      case (Some(_), None)    => JFilters.near(fieldName, geometry, maxDistance.get, null)
      case (None, Some(_))    => JFilters.near(fieldName, geometry, null, minDistance.get)
      case (None, None)       => JFilters.near(fieldName, geometry, null, null)
    }
  }
  /**
   * Creates a filter that matches all documents containing a field with geospatial data that is near the specified point.
   *
   * @param fieldName the field name
   * @param x the x coordinate
   * @param y the y coordinate
   * @return the filter
   * @see [[http://docs.mongodb.org/manual/reference/operator/query/near/ \$near]]
   */
  def near(fieldName: String, x: Double, y: Double): Bson = JFilters.near(fieldName, x, y, null, null)

  /**
   * Creates a filter that matches all documents containing a field with geospatial data that is near the specified point.
   *
   * @param fieldName the field name
   * @param x the x coordinate
   * @param y the y coordinate
   * @param maxDistance the optional maximum distance from the point, in radians
   * @param minDistance the optional minimum distance from the point, in radians
   * @return the filter
   * @see [[http://docs.mongodb.org/manual/reference/operator/query/near/ \$near]]
   */
  def near(fieldName: String, x: Double, y: Double, maxDistance: Option[Double], minDistance: Option[Double]): Bson = {
    (maxDistance, minDistance) match {
      case (Some(_), Some(_)) => JFilters.near(fieldName, x, y, maxDistance.get, minDistance.get)
      case (Some(_), None)    => JFilters.near(fieldName, x, y, maxDistance.get, null)
      case (None, Some(_))    => JFilters.near(fieldName, x, y, null, minDistance.get)
      case (None, None)       => JFilters.near(fieldName, x, y, null, null)
    }
  }

  /**
   * Creates a filter that matches all documents containing a field with geospatial data that is near the specified GeoJSON point using
   * spherical geometry.
   *
   * @param fieldName the field name
   * @param geometry the bounding GeoJSON geometry object
   * @return the filter
   * @see [[http://docs.mongodb.org/manual/reference/operator/query/near/ \$near]]
   */
  def nearSphere(fieldName: String, geometry: Point): Bson = JFilters.nearSphere(fieldName, geometry, null, null)

  /**
   * Creates a filter that matches all documents containing a field with geospatial data that is near the specified GeoJSON point using
   * spherical geometry.
   *
   * @param fieldName the field name
   * @param geometry the bounding GeoJSON geometry object
   * @param maxDistance the optional maximum distance from the point, in meters
   * @param minDistance the optional minimum distance from the point, in meters
   * @return the filter
   * @see [[http://docs.mongodb.org/manual/reference/operator/query/near/ \$near]]
   */
  def nearSphere(fieldName: String, geometry: Point, maxDistance: Option[Double], minDistance: Option[Double]): Bson = {
    (maxDistance, minDistance) match {
      case (Some(_), Some(_)) => JFilters.nearSphere(fieldName, geometry, maxDistance.get, minDistance.get)
      case (Some(_), None)    => JFilters.nearSphere(fieldName, geometry, maxDistance.get, null)
      case (None, Some(_))    => JFilters.nearSphere(fieldName, geometry, null, minDistance.get)
      case (None, None)       => JFilters.nearSphere(fieldName, geometry, null, null)
    }
  }

  /**
   * Creates a filter that matches all documents containing a field with geospatial data that is near the specified GeoJSON point using
   * spherical geometry.
   *
   * @param fieldName the field name
   * @param geometry the bounding GeoJSON geometry object
   * @return the filter
   * @see [[http://docs.mongodb.org/manual/reference/operator/query/near/ \$near]]
   */
  def nearSphere(fieldName: String, geometry: Bson): Bson = JFilters.nearSphere(fieldName, geometry, null, null)

  /**
   * Creates a filter that matches all documents containing a field with geospatial data that is near the specified GeoJSON point using
   * spherical geometry.
   *
   * @param fieldName the field name
   * @param geometry the bounding GeoJSON geometry object
   * @param maxDistance the optional maximum distance from the point, in meters
   * @param minDistance the optional minimum distance from the point, in meters
   * @return the filter
   * @see [[http://docs.mongodb.org/manual/reference/operator/query/near/ \$near]]
   */
  def nearSphere(fieldName: String, geometry: Bson, maxDistance: Option[Double], minDistance: Option[Double]): Bson = {
    (maxDistance, minDistance) match {
      case (Some(_), Some(_)) => JFilters.nearSphere(fieldName, geometry, maxDistance.get, minDistance.get)
      case (Some(_), None)    => JFilters.nearSphere(fieldName, geometry, maxDistance.get, null)
      case (None, Some(_))    => JFilters.nearSphere(fieldName, geometry, null, minDistance.get)
      case (None, None)       => JFilters.nearSphere(fieldName, geometry, null, null)
    }
  }

  /**
   * Creates a filter that matches all documents containing a field with geospatial data that is near the specified point using
   * spherical geometry.
   *
   * @param fieldName the field name
   * @param x the x coordinate
   * @param y the y coordinate
   * @return the filter
   * @see [[http://docs.mongodb.org/manual/reference/operator/query/near/ \$near]]
   */
  def nearSphere(fieldName: String, x: Double, y: Double): Bson = JFilters.nearSphere(fieldName, x, y, null, null)

  /**
   * Creates a filter that matches all documents containing a field with geospatial data that is near the specified point using
   * spherical geometry.
   *
   * @param fieldName the field name
   * @param x the x coordinate
   * @param y the y coordinate
   * @param maxDistance the optional maximum distance from the point, in radians
   * @param minDistance the optional minimum distance from the point, in radians
   * @return the filter
   * @see [[http://docs.mongodb.org/manual/reference/operator/query/near/ \$near]]
   */
  def nearSphere(fieldName: String, x: Double, y: Double, maxDistance: Option[Double], minDistance: Option[Double]): Bson = {
    (maxDistance, minDistance) match {
      case (Some(_), Some(_)) => JFilters.nearSphere(fieldName, x, y, maxDistance.get, minDistance.get)
      case (Some(_), None)    => JFilters.nearSphere(fieldName, x, y, maxDistance.get, null)
      case (None, Some(_))    => JFilters.nearSphere(fieldName, x, y, null, minDistance.get)
      case (None, None)       => JFilters.nearSphere(fieldName, x, y, null, null)
    }
  }
}
//scalastyle:on null number.of.methods
