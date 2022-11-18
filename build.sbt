
val sharedSettings = Seq(
  name := "mlang2",
  version := "0.1.0",
  scalaVersion := "3.2.1",
  scalacOptions ++= Seq("-Yexplicit-nulls"),
 // scalacOptions ++= Seq("-language:strictEquality", "-Ycheck-init"),
 // Test / classLoaderLayeringStrategy := ClassLoaderLayeringStrategy.ScalaLibrary,
  libraryDependencies ++= Seq(
    "com.lihaoyi" %% "os-lib" % "0.8.1",
    "org.scala-lang.modules" %% "scala-collection-contrib" % "0.3.0",
    "com.github.sbt" % "junit-interface" % "0.13.2" % Test,
    "com.lihaoyi" %% "pprint" % "0.7.1",
    "org.typelevel" %% "cats-core" % "2.9.0"
  ),
)

lazy val utils =
  project
 //  crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure)
  .in(file("src-utils"))
  .settings(
    sharedSettings,
  )

lazy val dench =
  project
 //  crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure)
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
 //  crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure)
  .in(file("src-mlang"))
  .settings(
    sharedSettings,
    libraryDependencies ++= Seq(
      "org.bytedeco" % "javacpp" % "1.5.8"
    )
  ).dependsOn(utils, dench)
  //.enablePlugins(ScalaJSBundlerPlugin)


/**
  * NOT ACTUALLY USED FOR NOW!!!
  */

val skijaVersion = "0.6.39"

val configureSkijaDependencyByPlatform = settingKey[ModuleID]("")

lazy val `dench-desktop` = project
  .in(file("src-dench-desktop"))
  .settings(
    sharedSettings,
    resolvers += "JetBrains" at "https://packages.jetbrains.team/maven/p/skija/maven",
    configureSkijaDependencyByPlatform := {
      System.getProperty("os.name").toLowerCase match {
        case str if str.contains("mac")  => "org.jetbrains.skija" % "skija-macos" % skijaVersion
        case str if str.contains("linux")  => "org.jetbrains.skija" % "skija-linux	" % skijaVersion
        case str if str.contains("win")  => "org.jetbrains.skija" % "skija-windows" % skijaVersion
        case str => throw new RuntimeException(s"Unknown operating system $str")
      }
    },
    libraryDependencies ++= Seq(
      configureSkijaDependencyByPlatform.value
    )
  )

// lazy val `dench-web` = project
//   .in(file("src-dench-web"))
//   .settings(
//     sharedSettings,
//     scalaJSUseMainModuleInitializer := true,
//     //mainClass in (Compile, run) := Some("mlang.ui.Main"),
//     libraryDependencies ++= Seq(
//     )
//   )
//   .enablePlugins(ScalaJSPlugin)
  //.dependsOn(dench.js)
