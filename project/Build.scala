import sbt._
import Keys._
import org.scalajs.sbtplugin.ScalaJSPlugin
import ScalaJSPlugin.autoImport._
import Lib._

object ScalaJsBenchmark {

  private val ghProject = "scalajs-benchmark"

  object Ver {
    val ChartJs       = "1.0.2"
    val MacroParadise = "2.1.0"
    val Monocle       = "1.3.2"
    val React         = "15.3.2"
    val Scala211      = "2.11.8"
    val Scala212      = "2.12.0"
    val ScalaCss      = "0.5.1"
    val ScalaJsReact  = "0.11.3"
  }

  def scalacFlags = Seq(
    "-deprecation", "-unchecked", "-feature",
    "-language:postfixOps", "-language:implicitConversions", "-language:higherKinds", "-language:existentials")

  val commonSettings: PE =
    _.settings(
      organization             := "com.github.japgolly.scalajs-benchmark",
      homepage                 := Some(url("https://github.com/japgolly/" + ghProject)),
      licenses                 += ("Apache-2.0", url("http://opensource.org/licenses/Apache-2.0")),
      scalaVersion             := Ver.Scala211,
      crossScalaVersions       := Seq(Ver.Scala211, Ver.Scala212),
      scalacOptions           ++= scalacFlags,
      scalacOptions           ++= byScalaVer(Seq.empty[String], Seq("-opt:l:method")).value,
      shellPrompt in ThisBuild := ((s: State) => Project.extract(s).currentRef.project + "> "),
      triggeredMessage         := Watched.clearWhenTriggered,
      incOptions               := incOptions.value.withNameHashing(true),
      updateOptions            := updateOptions.value.withCachedResolution(true))
    .configure(
      addCommandAliases(
        "/"   -> "project root",
        "C"   -> "root/clean",
        "T"   -> ";root/clean;root/test",
        "c"   -> "compile",
        "tc"  -> "test:compile",
        "t"   -> "test",
        "cc"  -> ";clean;compile",
        "ctc" -> ";clean;test:compile",
        "ct"  -> ";clean;test"))

  def definesMacros: Project => Project =
    _.settings(
      scalacOptions += "-language:experimental.macros",
      libraryDependencies ++= Seq(
        // "org.scala-lang" % "scala-reflect" % scalaVersion.value,
        // "org.scala-lang" % "scala-library" % scalaVersion.value,
        "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided"))

  def macroParadisePlugin =
    compilerPlugin("org.scalamacros" % "paradise" % Ver.MacroParadise cross CrossVersion.full)

  lazy val root =
    Project("root", file("."))
      .configure(commonSettings, preventPublication)
      .aggregate(benchmark, demo)

  lazy val benchmark =
    Project("benchmark", file("benchmark"))
      .enablePlugins(ScalaJSPlugin)
      .configure(commonSettings, definesMacros, publicationSettings(ghProject))
      .settings(
        libraryDependencies ++= Seq(
          "com.github.japgolly.scalajs-react" %%% "core"          % Ver.ScalaJsReact,
          "com.github.japgolly.scalajs-react" %%% "extra"         % Ver.ScalaJsReact,
          "com.github.japgolly.scalajs-react" %%% "ext-monocle"   % Ver.ScalaJsReact,
          "com.github.japgolly.scalacss"      %%% "core"          % Ver.ScalaCss,
          "com.github.japgolly.scalacss"      %%% "ext-react"     % Ver.ScalaCss,
          "com.github.julien-truffaut"        %%% "monocle-core"  % Ver.Monocle,
          "com.github.julien-truffaut"        %%% "monocle-macro" % Ver.Monocle),

        jsDependencies ++= Seq(
          "org.webjars.bower" % "react" % Ver.React
            /        "react-with-addons.js"
            minified "react-with-addons.min.js"
            commonJSName "React",

          "org.webjars.bower" % "react" % Ver.React
            /         "react-dom.js"
            minified  "react-dom.min.js"
            dependsOn "react-with-addons.js"
            commonJSName "ReactDOM",

          "org.webjars" % "chartjs" % Ver.ChartJs
            /        "Chart.js"
            minified "Chart.min.js"),

        addCompilerPlugin(macroParadisePlugin),
        test := ())

  object Demo {
    val outputJS = "output.js"
    val Cats      = "0.8.1"
    val Scalaz    = "7.2.7"
    val Shapeless = "2.3.2"

    def librariesFileTask = Def.task {
      val file = (sourceManaged in Compile).value / "demo" / "SbtLibraries.scala"
      val content = s"""
           |package demo
           |
           |trait SbtLibraries {
           |  final val Monocle   = Library("Monocle"  , "${Ver.Monocle}")
           |  final val Scala     = Library("Scala"    , "${scalaVersion.value}")
           |  final val Cats      = Library("Cats"     , "$Cats")
           |  final val Scalaz    = Library("Scalaz"   , "$Scalaz")
           |  final val Shapeless = Library("Shapeless", "$Shapeless")
           |}
         """.stripMargin
      IO.write(file, content)
      Seq(file)
    }
  }
  lazy val demo =
    Project("demo", file("demo"))
      .enablePlugins(ScalaJSPlugin)
      .configure(commonSettings, preventPublication)
      .dependsOn(benchmark)
      .settings(
        addCompilerPlugin(macroParadisePlugin),
        libraryDependencies ++= Seq(
          "org.scalaz"    %%% "scalaz-core"       % Demo.Scalaz,
          "org.scalaz"    %%% "scalaz-effect"     % Demo.Scalaz,
          "org.typelevel" %%% "cats"              % Demo.Cats,
          "com.chuusai"   %%% "shapeless"         % Demo.Shapeless),
        sourceGenerators in Compile += Demo.librariesFileTask.taskValue,
        skip in packageJSDependencies := false,
        artifactPath in (Compile, fastOptJS) := ((target in Compile).value / Demo.outputJS),
        artifactPath in (Compile, fullOptJS) := ((target in Compile).value / Demo.outputJS),
        test := ())
}
