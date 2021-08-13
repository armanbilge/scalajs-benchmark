package demo

import cats.effect.{ExitCode, IO, IOApp}
import demo.suites.scala.IntSet
import japgolly.scalajs.benchmark.Plan
import japgolly.scalajs.benchmark.engine.Engine
import demo.suites.scalajs.Allocation

object Main extends IOApp {

  override def run(args: List[String]): IO[ExitCode] =
    Engine.runToConsole(Plan(Allocation.suite, Vector(Allocation.Config(1000)))).as(ExitCode.Success)

}
