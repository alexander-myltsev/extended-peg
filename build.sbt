import com.typesafe.sbt.SbtScalariform.ScalariformKeys
import scalariform.formatter.preferences._
import sbt._
import Keys._

val commonSettings = Seq(
  version := "0.1",
  scalaVersion := "2.10.3",
  organization := "org.speg",
  homepage := None,
  description := "Staging experiments with PEG parsers gererator",
  startYear := Some(2014),
  licenses := Seq("Apache 2" -> new URL("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  javacOptions ++= Seq(
    "-encoding", "UTF-8",
    "-source", "1.6",
    "-target", "1.6",
    "-Xlint:unchecked",
    "-Xlint:deprecation"),
  scalacOptions ++= List(
    "-encoding", "UTF-8",
    "-feature",
    "-unchecked",
    "-deprecation",
    "-Xlint",
    "-language:_",
    "-target:jvm-1.6",
    "-Xlog-reflective-calls"),
  resolvers ++= Seq(Resolver.sonatypeRepo("snapshots"), Resolver.sonatypeRepo("releases")),
  shellPrompt := { s => Project.extract(s).currentProject.id + " > " })

val scalaReflect = "org.scala-lang"  %  "scala-reflect"    % "2.10.3"   % "compile"
val shapeless    = "com.chuusai"     %  "shapeless_2.10.3" % "2.0.0-M1" % "compile"
val specs2       = "org.specs2"      %% "specs2-core"      % "2.3.6"    % "test"

val formattingSettings = scalariformSettings ++ Seq(
  ScalariformKeys.preferences := ScalariformKeys.preferences.value
    .setPreference(RewriteArrowSymbols, true)
    .setPreference(AlignParameters, true)
    .setPreference(AlignSingleLineCaseStatements, true)
    .setPreference(DoubleIndentClassDeclaration, true)
    .setPreference(PreserveDanglingCloseParenthesis, true))

lazy val main = Project("main", file("."))
  .dependsOn(macroSub)
  .settings(commonSettings: _*)
  .settings(formattingSettings: _*)
  .settings(
    libraryDependencies ++= Seq(scalaReflect, shapeless, specs2))

lazy val macroSub = Project("macro", file("macro"))
  .settings(commonSettings: _*)
  .settings(formattingSettings: _*)
  .settings(
    addCompilerPlugin("org.scala-lang.plugins" % "macro-paradise" % "2.0.0-SNAPSHOT" cross CrossVersion.full),
    libraryDependencies ++= Seq(scalaReflect, shapeless, specs2))
