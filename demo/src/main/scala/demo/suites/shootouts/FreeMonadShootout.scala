package demo.suites.shootouts

import demo.Util._
import demo.suites
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.benchmark._
import japgolly.scalajs.benchmark.gui._
import monocle.Iso

object FreeMonadShootout {

  type BM = Benchmark[Int]
  sealed abstract class Lib(val name: String) {
    def freeFoldMap: BM
    def freeMapSuspension: BM
  }

  object Cats extends Lib("Cats") {
    override def freeFoldMap       = suites.cats.FreeMonads.bmFn0FoldMap
    override def freeMapSuspension = suites.cats.FreeMonads.bmFn0MapSuspension
  }

  object Scalaz extends Lib("Scalaz") {
    override def freeFoldMap       = suites.scalaz.FreeMonads.bmFn0FoldMap
    override def freeMapSuspension = suites.scalaz.FreeMonads.bmFn0MapSuspension
  }

  case class Params(lib: Lib, size: Int) {
    override def toString = s"${lib.name} @ $size"
  }

  val param1 = GuiParam.enum[Lib]("Library", Cats, Scalaz)(_.name)
  val param2 = GuiParam.int("Size", 500)

  val iso = Iso((m: Params) => Params.unapply(m).get)((Params.apply _).tupled)
  val params = GuiParams.two(iso, param1, param2)

  val suite = Suite[Params]("Free monads")(
    Benchmark.derive("Free → Fn0 (foldMap)",       (_: Params).lib.freeFoldMap      )(_.size),
    Benchmark.derive("Free → Fn0 (mapSuspension)", (_: Params).lib.freeMapSuspension)(_.size))

  val guiSuite = GuiSuite(suite, params).describe(
    <.div(
      <.div("Shootout between free monad implementations."),
      linkToSource(sourceFilename)))
}
