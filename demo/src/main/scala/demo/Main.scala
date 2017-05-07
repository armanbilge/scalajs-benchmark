package demo

import org.scalajs.dom.document
import japgolly.scalajs.benchmark.gui.BenchmarkGUI

object Main extends scalajs.js.JSApp {

  def main(): Unit = {

    // import concurrent.duration._
    // import japgolly.scalajs.benchmark.engine.Options
    // val opts = Options.Default.copy(minRuns = 1000, minTime = 0.millis)

    val tgt = document.getElementById("body")

    BenchmarkGUI.renderMenu(tgt, layout = configureLayout)(
      suites.example.Examples.all,
      suites.cats.all,
      suites.scala.all,
      suites.scalaz.all,
      suites.shootouts.all)
  }

  /*
   * Customise the layout slightly.
   *
   * Optional, of course.
   */
  import japgolly.scalajs.benchmark.gui.MenuComp.LayoutCfg
  import japgolly.scalajs.react._, vdom.html_<^._
  import demo.Util._
  def configureLayout: LayoutCfg = {
    def top(view: VdomElement): VdomElement =
      <.main(
        <.h1(
          ^.marginBottom := "0.2em",
          "Benchmark Collection"),
        <.div(
          ^.marginBottom := "0.5ex",
          ^.fontSize := "0.8em",
          ^.fontFamily := "monospace",
          s"Generated with ${Libraries.ScalaJs.fullName}."),
        <.div(
          linkToSource(sourceFilename)(^.textAlign.left)),
        <.main(
          ^.marginTop := "2.6em",
          view))
    LayoutCfg.default.copy(topPage = top)
  }
}
