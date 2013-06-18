scalaVersion := "2.10.2"

libraryDependencies ++= Seq("org.specs2" %% "specs2" % "1.14" % "test",
			    "org.scalacheck" %% "scalacheck" % "1.10.1" % "test")

resolvers ++= Seq("snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
	          "releases"  at "http://oss.sonatype.org/content/repositories/releases")
