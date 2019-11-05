name := "lia"

version := "0.23.1"

scalaVersion := "2.13.1"

scalacOptions ++= Seq( "-deprecation", "-feature", "-language:postfixOps", "-language:implicitConversions", "-language:existentials" )

organization := "xyz.hyperreal"

resolvers += "Hyperreal Repository" at "https://dl.bintray.com/edadma/maven"

libraryDependencies ++= Seq(
	"org.scalatest" %% "scalatest" % "3.0.8" % "test",
	"org.scalacheck" %% "scalacheck" % "1.14.0" % "test"
)

libraryDependencies += "xyz.hyperreal" %% "numbers" % "0.7.1"

publishMavenStyle := true

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

licenses := Seq("ISC" -> url("http://opensource.org/licenses/ISC"))

homepage := Some(url("https://github.com/edadma/lia"))

pomExtra :=
  <scm>
    <url>git@github.com:edadma/lia.git</url>
    <connection>scm:git:git@github.com:edadma/lia.git</connection>
  </scm>
  <developers>
    <developer>
      <id>edadma</id>
      <name>Edward A. Maxedon, Sr.</name>
      <url>http://funl-lang.org</url>
    </developer>
  </developers>
