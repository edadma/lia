name := "lia"

version := "0.23"

scalaVersion := "2.13.0"

scalacOptions ++= Seq( "-deprecation", "-feature", "-language:postfixOps", "-language:implicitConversions", "-language:existentials" )

organization := "xyz.hyperreal"

resolvers += "Hyperreal Repository" at "https://dl.bintray.com/edadma/maven"

libraryDependencies ++= Seq(
	"org.scalatest" %% "scalatest" % "3.0.8" % "test",
	"org.scalacheck" %% "scalacheck" % "1.14.0" % "test"
)

libraryDependencies += "xyz.hyperreal" %% "numbers" % "0.7"


publishMavenStyle := true

//publishTo := Some( Resolver.sftp( "Hyperreal Repository", "hyperreal.ca", "/var/www/hyperreal.ca/maven2" ) )

//{
//  val nexus = "https://oss.sonatype.org/"
//  if (isSnapshot.value)
//    Some("snapshots" at nexus + "content/repositories/snapshots")
//  else
//    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
//}

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
