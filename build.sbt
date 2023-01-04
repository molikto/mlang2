
val sharedSettings = Seq(
  name := "mlang2",
  version := "0.1.0",
  scalaVersion := "3.2.1",
  scalacOptions ++= Seq("-Yexplicit-nulls"),
  //scalacOptions ++= Seq("-language:strictEquality", "-Ycheck-init"),
 // Test / classLoaderLayeringStrategy := ClassLoaderLayeringStrategy.ScalaLibrary,
  libraryDependencies ++= Seq(
    "com.lihaoyi" %% "os-lib" % "0.8.1",
    "org.scala-lang.modules" %% "scala-collection-contrib" % "0.3.0",
    "com.github.sbt" % "junit-interface" % "0.13.2" % Test,
    "com.lihaoyi" %% "pprint" % "0.7.1"
  ),
)

lazy val utils =
  project
  .in(file("src-utils"))
  .settings(
    sharedSettings,
  )

lazy val dench =
  project
  .in(file("src-dench"))
  .settings(
    sharedSettings,
    libraryDependencies ++= Seq(
      "org.bytedeco" % "javacpp" % "1.5.8"
    )
  )
  .dependsOn(utils)

lazy val mlang =
  project
  .in(file("src-mlang"))
  .settings(
    sharedSettings,
    libraryDependencies ++= Seq(
      "org.bytedeco" % "javacpp" % "1.5.8"
    )
  ).dependsOn(utils, dench)

