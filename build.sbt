name := "erbs-route"

ThisBuild / organization := "_2ne1ugly"
ThisBuild / version := "0.0.1"
ThisBuild / scalaVersion := "2.13.4"
ThisBuild / scalacOptions ++= Seq(
  "-target:jvm-1.8",
  "-unchecked",
  "-deprecation",
  "-feature",
  "-Xlint:nullary-unit,inaccessible,infer-any,missing-interpolator,private-shadow,type-parameter-shadow,poly-implicit-overload,option-implicit,delayedinit-select,stars-align",
  "-Xcheckinit",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-Ymacro-annotations",
  "-encoding",
  "utf8"
)

lazy val root = (project in file("."))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    libraryDependencies ++= Seq(
      "com.raquo"     %%% "laminar"   % "0.11.0",
      "com.raquo"     %%% "airstream" % "0.11.1",
      "org.typelevel" %%% "cats-core" % "2.1.1"
    ),
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) },
    addCommandAlias("dev", ";~fastOptJS")
  )
