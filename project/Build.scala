import sbt._
import Keys._
import com.typesafe.sbt.pgp.PgpKeys
import org.scalajs.jsdependencies.sbtplugin.JSDependenciesPlugin
import org.scalajs.jsdependencies.sbtplugin.JSDependenciesPlugin.autoImport._
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv
import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._
import sbtrelease.ReleasePlugin.autoImport._
import Lib._

object ScalaJsBenchmark {

  private val ghProject = "scalajs-benchmark"

  object Ver {
    val BetterMonadicFor = "0.3.1"
    val ChartJs          = "1.0.2"
    val Circe            = "0.13.0"
    val MacroParadise    = "2.1.1"
    val Monocle          = "1.6.3"
    val React            = "16.13.1"
    val Scala212         = "2.12.11"
    val Scala213         = "2.13.2"
    val ScalaCollCompat  = "2.1.6"
    val ScalaCss         = "0.6.1"
    val ScalaJsReact     = "1.7.0"
    val Scalaz           = "7.2.30"

    // Test only
    val Microlibs = "2.3"
    val MTest     = "0.7.4"

    // Demo only
    val Cats      = "2.1.1"
    val Shapeless = "2.3.3"
  }

  def scalacFlags = Seq(
    "-deprecation",
    "-unchecked",
    "-feature",
    "-language:postfixOps",
    "-language:implicitConversions",
    "-language:higherKinds",
    "-language:existentials",
    "-opt:l:method")

  val commonSettings: PE =
    _.settings(
      organization                  := "com.github.japgolly.scalajs-benchmark",
      homepage                      := Some(url("https://github.com/japgolly/" + ghProject)),
      licenses                      += ("Apache-2.0", url("http://opensource.org/licenses/Apache-2.0")),
      scalaVersion                  := Ver.Scala213,
      crossScalaVersions            := Seq(Ver.Scala212, Ver.Scala213),
      scalacOptions                ++= scalacFlags,
      shellPrompt in ThisBuild      := ((s: State) => Project.extract(s).currentRef.project + "> "),
      incOptions                    := incOptions.value.withLogRecompileOnMacro(false),
      updateOptions                 := updateOptions.value.withCachedResolution(true),
      releasePublishArtifactsAction := PgpKeys.publishSigned.value,
      releaseTagComment             := s"v${(version in ThisBuild).value}",
      releaseVcsSign                := true,
      addCompilerPlugin(compilerPlugin("com.olegpy" %% "better-monadic-for" % Ver.BetterMonadicFor)))

  def definesMacros: Project => Project =
    _.settings(
      scalacOptions += "-language:experimental.macros",
      libraryDependencies ++= Seq(
        // "org.scala-lang" % "scala-reflect" % scalaVersion.value,
        // "org.scala-lang" % "scala-library" % scalaVersion.value,
        "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided"))

  def addMacroParadisePlugin = Def.settings(
    Seq(
      libraryDependencies ++= byScalaVersion {
        case (2, 12) => Seq(compilerPlugin("org.scalamacros" % "paradise" % Ver.MacroParadise cross CrossVersion.patch))
        case (2, 13) => Nil
      }.value,
      scalacOptions ++= byScalaVersion {
        case (2, 12) => Nil
        case (2, 13) => Seq("-Ymacro-annotations")
      }.value
    ))

  def utestSettings: PE =
    _.settings(
      jsEnv               := new JSDOMNodeJSEnv,
      libraryDependencies += "com.lihaoyi" %%% "utest" % Ver.MTest % Test,
      libraryDependencies += "com.github.japgolly.microlibs" %%% "test-util" % Ver.Microlibs % Test,
      testFrameworks      := new TestFramework("utest.runner.Framework") :: Nil)

  lazy val genBoilerplate = TaskKey[Unit]("genBoilerplate")

  lazy val root =
    Project("root", file("."))
      .configure(commonSettings, preventPublication)
      .aggregate(benchmark, demo)

  // ===================================================================================================================

  lazy val benchmark =
    Project("benchmark", file("benchmark"))
      .enablePlugins(ScalaJSPlugin)
      .enablePlugins(JSDependenciesPlugin)
      .configure(commonSettings, definesMacros, publicationSettings(ghProject), utestSettings)
      .settings(
        addMacroParadisePlugin,

        libraryDependencies ++= Seq(
          "org.scala-lang.modules"            %%% "scala-collection-compat" % Ver.ScalaCollCompat,
          "com.github.japgolly.scalajs-react" %%% "core"                    % Ver.ScalaJsReact,
          "com.github.japgolly.scalajs-react" %%% "extra"                   % Ver.ScalaJsReact,
          "com.github.japgolly.scalajs-react" %%% "ext-monocle-scalaz"      % Ver.ScalaJsReact,
          "com.github.japgolly.scalacss"      %%% "core"                    % Ver.ScalaCss,
          "com.github.japgolly.scalacss"      %%% "ext-react"               % Ver.ScalaCss,
          "com.github.julien-truffaut"        %%% "monocle-core"            % Ver.Monocle,
          "com.github.julien-truffaut"        %%% "monocle-macro"           % Ver.Monocle,
          "io.circe"                          %%% "circe-core"              % Ver.Circe,
          "io.circe"                          %%% "circe-generic"           % Ver.Circe,
          "io.circe"                          %%% "circe-parser"            % Ver.Circe % Test,
          "org.scalaz"                        %%% "scalaz-core"             % Ver.Scalaz),

        dependencyOverrides += "org.webjars.npm" % "js-tokens" % "3.0.2", // https://github.com/webjars/webjars/issues/1789

        jsDependencies ++= Seq(
          "org.webjars.npm" % "react" % Ver.React
            /        "umd/react.development.js"
            minified "umd/react.production.min.js"
            commonJSName "React",

          "org.webjars.npm" % "react-dom" % Ver.React
            /         "umd/react-dom.development.js"
            minified  "umd/react-dom.production.min.js"
            dependsOn "umd/react.development.js"
            commonJSName "ReactDOM",

          "org.webjars.npm" % "jstat" % "1.9.3"
            /        "dist/jstat.js"
            minified "dist/jstat.min.js",

          "org.webjars.npm" % "file-saver" % "2.0.2"
            /        "dist/FileSaver.js"
            minified "dist/FileSaver.min.js",

          "org.webjars" % "chartjs" % Ver.ChartJs
            /        "Chart.js"
            minified "Chart.min.js"),

        genBoilerplate := GenBoilerplate(sourceDirectory.value / "main" / "scala")
      )

  // ===================================================================================================================

  object Demo {
    def librariesFileTask = Def.task {
      val file = (sourceManaged in Compile).value / "demo" / "SbtLibraries.scala"
      val content = s"""
           |package demo
           |
           |trait SbtLibraries {
           |  final val Monocle   = Library("Monocle"  , "${Ver.Monocle}")
           |  final val Scala     = Library("Scala"    , "${scalaVersion.value}")
           |  final val Cats      = Library("Cats"     , "${Ver.Cats}")
           |  final val Scalaz    = Library("Scalaz"   , "${Ver.Scalaz}")
           |  final val Shapeless = Library("Shapeless", "${Ver.Shapeless}")
           |}
         """.stripMargin
      IO.write(file, content)
      Seq(file)
    }
  }

  lazy val demo =
    Project("demo", file("demo"))
      .enablePlugins(ScalaJSPlugin)
      .enablePlugins(JSDependenciesPlugin)
      .configure(commonSettings, preventPublication)
      .dependsOn(benchmark)
      .settings(
        addMacroParadisePlugin,
        libraryDependencies ++= Seq(
          "org.scalaz"    %%% "scalaz-core"   % Ver.Scalaz,
          "org.scalaz"    %%% "scalaz-effect" % Ver.Scalaz,
          "org.typelevel" %%% "cats-core"     % Ver.Cats,
          "org.typelevel" %%% "cats-free"     % Ver.Cats,
          "com.chuusai"   %%% "shapeless"     % Ver.Shapeless),
        sourceGenerators in Compile += Demo.librariesFileTask.taskValue,
        scalaJSLinkerConfig ~= { _.withSourceMap(true) },
        skip in packageJSDependencies := false,
        test := { (compile in Test).value; () })
}
