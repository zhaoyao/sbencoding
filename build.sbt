import SonatypeKeys._
import scalariform.formatter.preferences._

name := "sbencoding"

organization := "com.github.zhaoyao"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.11.6"

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2-core" % "2.4.16" % "test",
  "org.specs2" %% "specs2-scalacheck" % "2.4.16" % "test",
  "org.scalacheck" %% "scalacheck" % "1.12.2" % "test"
)

scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation",
  "-Xlint",
  "-Ywarn-dead-code",
  "-feature",
  //  "-Ylog-classpath",
  "-language:_",
  "-target:jvm-1.7",
  "-encoding", "UTF-8"
)

// generate boilerplate
Boilerplate.settings

publishMavenStyle := true

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

pomIncludeRepository := { _ => false }

pomExtra := (
  <url>http://zhaoyao.github.io/sbencoding/</url>
    <licenses>
      <license>
        <name>MIT</name>
        <url>http://opensource.org/licenses/MIT</url>
        <distribution>repo</distribution>
      </license>
    </licenses>
    <scm>
      <url>git@github.com:zhaoyao/sbencoding.git</url>
      <connection>scm:git:git@github.com:zhaoyao/sbencoding.git</connection>
    </scm>
    <developers>
      <developer>
        <id>zhaoyao</id>
        <name>Yao Zhao</name>
      </developer>
    </developers>)

xerial.sbt.Sonatype.sonatypeSettings

scalariformSettings

ScalariformKeys.preferences := ScalariformKeys.preferences.value
  .setPreference(AlignParameters, true)
  .setPreference(AlignSingleLineCaseStatements, true)
  .setPreference(DoubleIndentClassDeclaration, true)
  .setPreference(PreserveDanglingCloseParenthesis, true)
  .setPreference(SpacesWithinPatternBinders, true)

lazy val sbencoding = project.in(file("."))