import sbt._
object Sword extends Build {
	lazy val root = Project("", file(".")) dependsOn(dispatchLiftJson)
	lazy val dispatchLiftJson = uri("git://github.com/dispatch/dispatch-lift-json#0.1.1")
}