name := "cats-sandbox"
version := "0.0.1-SNAPSHOT"

scalaVersion := "2.13.2"

libraryDependencies += "org.typelevel" %% "cats-core" % "2.0.0"
libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value // it's to type level tutorial
libraryDependencies ++= Seq(
  "com.github.julien-truffaut" %% "monocle-core"  % "2.0.3",
  "com.github.julien-truffaut" %% "monocle-macro" % "2.0.3",
)

// scalac options come from the sbt-tpolecat plugin so need to set any here
scalacOptions in Global += "-Ymacro-annotations"
scalacOptions in Compile --= Seq("-Ywarn-unused:imports", "-Xfatal-warnings")
addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)

