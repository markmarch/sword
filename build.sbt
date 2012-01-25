name := "sword"

version := "0.1"

scalaVersion := "2.9.1"

resolvers ++= Seq("snapshots" at "http://scala-tools.org/repo-snapshots",
					"releases" at "http://scala-tools.org/repo-releases")

libraryDependencies ++= Seq(
	"net.databinder" %% "dispatch-core" % "0.8.7",
	"net.databinder" %% "dispatch-http-json" % "0.8.7",
	"net.databinder" %% "dispatch-oauth" % "0.8.7",
	"org.specs2" %% "specs2" % "1.7.1" % "test->default"
)