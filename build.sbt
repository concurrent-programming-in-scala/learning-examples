
name := "concurrency-examples"

version := "1.0"

scalaVersion := "2.10.3"

resolvers ++= Seq(
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
  "Sonatype OSS Releases" at "https://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies += "commons-io" % "commons-io" % "2.4"

libraryDependencies += "org.scala-lang.modules" %% "scala-async" % "0.9.1"

libraryDependencies += "com.netflix.rxjava" % "rxjava-scala" % "0.17.2"

libraryDependencies += "org.scala-lang" % "scala-swing" % "2.10.3"
            

