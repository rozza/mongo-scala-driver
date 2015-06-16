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

import scala.collection.mutable.ArrayBuffer
import scala.collection.JavaConverters._

import com.mongodb.client.model.{ geojson => Jgeojson }

package object geojson {

  /**
   * A GeoJSON Coordinate Reference System (CRS).
   */
  type CoordinateReferenceSystem = Jgeojson.CoordinateReferenceSystem

  /**
   * GeoJSON coordinate reference system types.
   */
  type CoordinateReferenceSystemType = Jgeojson.CoordinateReferenceSystemType

  object CoordinateReferenceSystemType {
    /**
     * A coordinate reference system that is specifed by name
     */
    val NAME = Jgeojson.CoordinateReferenceSystemType.NAME

    /**
     * A coordinate reference system that is specifed by a dereferenceable URI
     */
    val LINK = Jgeojson.CoordinateReferenceSystemType.LINK
  }

  /**
   * The GeoJSON object types.
   */
  type GeoJsonObjectType = Jgeojson.GeoJsonObjectType
  object GeoJsonObjectType {
    /**
     * A GeometryCollection
     */
    val GEOMETRY_COLLECTION = Jgeojson.GeoJsonObjectType.GEOMETRY_COLLECTION

    /**
     * A LineString
     */
    val LINE_STRING = Jgeojson.GeoJsonObjectType.LINE_STRING

    /**
     * A MultiLineString
     */
    val MULTI_LINE_STRING = Jgeojson.GeoJsonObjectType.MULTI_LINE_STRING

    /**
     * A MultiPoint
     */
    val MULTI_POINT = Jgeojson.GeoJsonObjectType.MULTI_POINT

    /**
     * A MultiPolygon
     */
    val MULTI_POLYGON = Jgeojson.GeoJsonObjectType.MULTI_POLYGON

    /**
     * A Point
     */
    val POINT = Jgeojson.GeoJsonObjectType.POINT

    /**
     * A Polygon
     */
    val POLYGON = Jgeojson.GeoJsonObjectType.POLYGON
  }

  /**
   * An abstract class for representations of GeoJSON geometry objects.
   */
  type Geometry = Jgeojson.Geometry

  /**
   * A representation of a GeoJSON GeometryCollection.
   */
  type GeometryCollection = Jgeojson.GeometryCollection
  object GeometryCollection {
    /**
     * Construct an instance with the given list of Geometry objects
     *
     * @param geometries  the list of Geometry objects
     */
    def apply(geometries: List[geojson.Geometry]): GeometryCollection = new Jgeojson.GeometryCollection(geometries.asJava)

    /**
     * Construct an instance with the given list of Geometry objects
     *
     * @param coordinateReferenceSystem the coordinate reference system
     * @param geometries  the list of Geometry objects
     */
    def apply(coordinateReferenceSystem: CoordinateReferenceSystem, geometries: List[Geometry]): GeometryCollection =
      new Jgeojson.GeometryCollection(coordinateReferenceSystem, geometries.asJava)
  }

  /**
   * A representation of a GeoJSON LineString.
   */
  type LineString = Jgeojson.LineString
  object LineString {
    /**
     * Construct an instance with the given coordinates.
     *
     * @param coordinates  the list of Geometry objects
     */
    def apply(coordinates: List[Position]): LineString = new Jgeojson.LineString(coordinates.asJava)

    /**
     * Construct an instance with the given coordinates.
     *
     * @param coordinateReferenceSystem the coordinate reference system
     * @param coordinates  the list of Geometry objects
     */
    def apply(coordinateReferenceSystem: CoordinateReferenceSystem, coordinates: List[Position]): LineString =
      new Jgeojson.LineString(coordinateReferenceSystem, coordinates.asJava)
  }

  /**
   * A representation of a GeoJSON MultiLineString.
   */
  type MultiLineString = Jgeojson.MultiLineString
  object MultiLineString {

    /**
     * Construct an instance with the given coordinates.
     *
     * @param coordinates the coordinates of each line
     */
    def apply(coordinates: List[List[Position]]): MultiLineString = new MultiLineString(coordinates.map(_.asJava).asJava)

    /**
     * Construct an instance with the given coordinates and coordinate reference system.
     *
     * @param coordinateReferenceSystem the coordinate reference system
     * @param coordinates the coordinates of each line
     */
    def apply(coordinateReferenceSystem: CoordinateReferenceSystem, coordinates: List[List[Position]]): MultiLineString =
      new Jgeojson.MultiLineString(coordinateReferenceSystem, coordinates.map(_.asJava).asJava)
  }

  /**
   * A representation of a GeoJSON MultiPoint.
   */
  type MultiPoint = Jgeojson.MultiPoint
  object MultiPoint {

    /**
     * Construct an instance with the given coordinates.
     *
     * @param coordinates the coordinates
     */
    def apply(coordinates: List[Position]): MultiPoint = new Jgeojson.MultiPoint(coordinates.asJava)

    /**
     * Construct an instance with the given coordinates and coordinate reference system.
     *
     * @param coordinateReferenceSystem the coordinate reference system
     * @param coordinates               the coordinates
     */
    def apply(coordinateReferenceSystem: CoordinateReferenceSystem, coordinates: List[Position]): MultiPoint =
      new Jgeojson.MultiPoint(coordinateReferenceSystem, coordinates.asJava)
  }

  /**
   * A representation of a GeoJSON MultiPolygon.
   */
  type MultiPolygon = Jgeojson.MultiPolygon
  object MultiPolygon {
    /**
     * Construct an instance.
     *
     * @param coordinates the coordinates
     */
    def apply(coordinates: List[PolygonCoordinates]): MultiPolygon = new Jgeojson.MultiPolygon(coordinates.asJava)

    /**
     * Construct an instance.
     *
     * @param coordinateReferenceSystem the coordinate reference system
     * @param coordinates the coordinates
     */
    def apply(coordinateReferenceSystem: CoordinateReferenceSystem, coordinates: List[PolygonCoordinates]): MultiPolygon =
      new Jgeojson.MultiPolygon(coordinateReferenceSystem, coordinates.asJava)
  }

  /**
   * A GeoJSON named Coordinate Reference System.
   */
  type NamedCoordinateReferenceSystem = Jgeojson.NamedCoordinateReferenceSystem
  object NamedCoordinateReferenceSystem {
    /**
     * The EPSG:4326 Coordinate Reference System.
     */
    val EPSG_4326: NamedCoordinateReferenceSystem = Jgeojson.NamedCoordinateReferenceSystem.EPSG_4326
    /**
     * The urn:ogc:def:crs:OGC:1.3:CRS84 Coordinate Reference System
     */
    val CRS_84: NamedCoordinateReferenceSystem = Jgeojson.NamedCoordinateReferenceSystem.CRS_84
    /**
     * A custom MongoDB EPSG:4326 Coordinate Reference System that uses a strict counter-clockwise winding order.
     *
     * [[http://docs.mongodb.org/manual/reference/operator/query/geometry/ Strict Winding]]
     */
    val EPSG_4326_STRICT_WINDING: NamedCoordinateReferenceSystem = Jgeojson.NamedCoordinateReferenceSystem.EPSG_4326_STRICT_WINDING

    /**
     * Construct an instance
     *
     * @param name the name
     */
    def apply(name: String): NamedCoordinateReferenceSystem = new Jgeojson.NamedCoordinateReferenceSystem(name)
  }

  /**
   * A representation of a GeoJSON Point.
   */
  type Point = Jgeojson.Point
  object Point {
    /**
     * Construct an instance with the given coordinate.
     *
     * @param coordinate the non-null coordinate of the point
     */
    def apply(coordinate: Position): Point = new Jgeojson.Point(coordinate)

    /**
     * Construct an instance with the given coordinate and coordinate reference system.
     *
     * @param coordinateReferenceSystem the coordinate reference system
     * @param coordinate the non-null coordinate of the point
     */
    def apply(coordinateReferenceSystem: CoordinateReferenceSystem, coordinate: Position): Point =
      new Jgeojson.Point(coordinateReferenceSystem, coordinate)
  }

  /**
   * A representation of a GeoJSON Polygon.
   */
  type Polygon = Jgeojson.Polygon
  object Polygon {

    /**
     * Construct an instance with the given coordinates.
     *
     * @param exterior the exterior ring of the polygon
     * @param holes    optional interior rings of the polygon
     */
    def apply(exterior: List[Position], holes: List[Position]*): Polygon = new Jgeojson.Polygon(exterior.asJava, holes.map(_.asJava): _*)

    /**
     * Construct an instance with the given coordinates.
     *
     * @param coordinates the coordinates
     */
    def apply(coordinates: PolygonCoordinates): Polygon = new Jgeojson.Polygon(coordinates)

    /**
     * Construct an instance with the given coordinates and coordinate reference system.
     *
     * @param coordinateReferenceSystem the coordinate reference system
     * @param coordinates               the coordinates
     */
    def apply(coordinateReferenceSystem: CoordinateReferenceSystem, coordinates: PolygonCoordinates): Polygon =
      new Jgeojson.Polygon(coordinateReferenceSystem, coordinates)
  }

  /**
   * Coordinates for a GeoJSON Polygon.
   */
  type PolygonCoordinates = Jgeojson.PolygonCoordinates
  object PolygonCoordinates {
    /**
     * Construct an instance.
     *
     * @param exterior the exterior ring of the polygon
     * @param holes    optional interior rings of the polygon
     */
    def apply(exterior: List[Position], holes: List[Position]*): PolygonCoordinates =
      new Jgeojson.PolygonCoordinates(exterior.asJava, holes.map(_.asJava): _*)
  }

  /**
   * A representation of a GeoJSON Position.
   */
  type Position = Jgeojson.Position
  object Position {
    def apply(values: List[Double]): Position = {
      new Jgeojson.Position(values.map(_.asInstanceOf[java.lang.Double]).asJava)
    }

    def apply(first: Double, second: Double, remaining: Double*): Position = {
      val buffer = new ArrayBuffer[java.lang.Double]
      buffer.append(first)
      buffer.append(second)
      buffer.append(remaining.map(_.asInstanceOf[java.lang.Double]): _*)
      new Jgeojson.Position(buffer.asJava)
    }
  }
}
