package demo

import japgolly.scalajs.benchmark.Plan
import demo.suites.scala.IntSet
import japgolly.scalajs.benchmark.BenchmarkIOApp

object Main extends BenchmarkIOApp[Int] {
  override def plan = Plan(IntSet.suite, Vector(10000))
}
