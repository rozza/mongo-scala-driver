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

import com.typesafe.sbt.SbtScalariform._
import org.scalastyle.sbt.ScalastylePlugin._
import sbt.Keys._
import sbt._
import sbtassembly.Plugin.AssemblyKeys._
import sbtassembly.Plugin._
import scoverage.ScoverageSbtPlugin._

object MongoScalaBuild extends Build {

  import Dependencies._
  import Resolvers._

  val buildSettings = Seq(
    organization := "org.mongodb.scala",
    organizationHomepage := Some(url("http://www.mongodb.org")),
    version := "1.0.0-SNAPSHOT",
    scalaVersion := scalaCoreVersion,
    libraryDependencies ++= coreDependencies ++ testDependencies,
    resolvers := mongoScalaResolvers,
    scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Xlint" /*, "-Xlog-implicits", "-Yinfer-debug", "-Xprint:typer"*/),
    scalacOptions in(Compile, doc) ++= Seq("-diagrams", "-unchecked", "-doc-root-content", "driver/rootdoc.txt")
  )

  val publishSettings = Publish.settings

  /*
   * Test Settings
   */
  val testSettings = Seq(
    testFrameworks += TestFrameworks.ScalaTest,
    testOptions in Test += Tests.Argument("-oD"),
    ScoverageKeys.coverageMinimum := 95,
    ScoverageKeys.coverageFailOnMinimum := true
  )

  val scoverageSettings = Seq()

  /*
   * Style and formatting
   */
  def scalariFormFormattingPreferences = {
    import scalariform.formatter.preferences._
    FormattingPreferences()
      .setPreference(AlignParameters, true)
      .setPreference(AlignSingleLineCaseStatements, true)
      .setPreference(DoubleIndentClassDeclaration, true)
  }

  val customScalariformSettings = scalariformSettings ++ Seq(
    ScalariformKeys.preferences in Compile := scalariFormFormattingPreferences,
    ScalariformKeys.preferences in Test    := scalariFormFormattingPreferences
  )

  val scalaStyleSettings = Seq(
    (scalastyleConfig in Compile) := file("project/scalastyle-config.xml")
  )

  /*
   * Assembly Jar Settings
   */
  val driverAssemblyJarSettings = assemblySettings ++
    addArtifact(Artifact("mongo-scala-driver-alldep", "jar", "jar"), assembly) ++ Seq(test in assembly := {})

  lazy val driver = Project(
    id = "driver",
    base = file("driver")
  ).settings(buildSettings: _*)
    .settings(testSettings: _*)
    .settings(publishSettings: _*)
    .settings(driverAssemblyJarSettings: _*)
    .settings(customScalariformSettings: _*)
    .settings(scalaStyleSettings: _*)
    .settings(scoverageSettings: _*)
    .settings(initialCommands in console := """import com.mongodb.scala._""")
    .dependsOn(core)

  lazy val core = Project(
    id = "core",
    base = file("core")
  ).settings(buildSettings: _*)
    .settings(testSettings: _*)
    .settings(scalaVersion := scalaCoreVersion)

  lazy val root = Project(
    id = "root",
    base = file(".")
  ).aggregate(core)
    .aggregate(driver)
    .settings(buildSettings: _*)
    .settings(scalaStyleSettings: _*)
    .settings(scoverageSettings: _*)
    .settings(publish := {}, publishLocal := {})

  override def rootProject = Some(root)

}
