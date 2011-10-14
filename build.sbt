// import web plugin settings
seq(webSettings :_*)

name := "docserver"

version := "1.0-SNAPSHOT"

organization := "com.samskivert"

scalaVersion := "2.9.1"

scalacOptions ++= Seq("-unchecked", "-deprecation")

libraryDependencies ++= Seq(
  "com.samskivert" % "jmustache" % "1.5-SNAPSHOT",
  "com.samskivert" % "samskivert" % "1.5",
  "org.eclipse.jetty" % "jetty-webapp" % "8.0.1.v20110908" % "container",
  "javax.servlet" % "servlet-api" % "2.5" % "provided->default"
)
