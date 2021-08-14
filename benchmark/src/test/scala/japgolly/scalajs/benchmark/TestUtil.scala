package japgolly.scalajs.benchmark

import japgolly.scalajs.benchmark.engine._
import scala.concurrent.duration._
import cats.kernel.Eq
import cats.syntax.all._

object TestUtil {

  implicit def durationToMs(d: Duration): Double =
    TimeUtil.toMs(d)

  implicit def msToDuration(d: Double): Duration =
    TimeUtil.fromMs(d)

  def stats(durs: Duration*): Stats =
    Stats(durs.iterator.map(IterationStats(1, _)).toVector)

  def statPlusMinus(dur: FiniteDuration, pm: FiniteDuration) =
    stats(dur, dur + pm, dur - pm)

  def statMatrix(iterations: Int, opsPerIteration: Int)(
      f: (Int, Int) => Duration
  ): Stats =
    Stats(Vector.tabulate(iterations) { i =>
      val sum =
        (1 to opsPerIteration).iterator.map(f(i, _)).map(TimeUtil.toMs).sum
      IterationStats(opsPerIteration, sum)
    })

  def itStats(times: Double*): IterationStats = {
    val b = new IterationStats.Builder
    times.foreach(b.add)
    b.result()
  }

  def stats(i1: IterationStats, in: IterationStats*): Stats =
    Stats(i1 +: in.toVector)

  def assertEq[A: Eq](actual: A, expect: A): Unit =
    assertEqO(None, actual, expect)

  def assertEq[A: Eq](name: => String, actual: A, expect: A): Unit =
    assertEqO(Some(name), actual, expect)

  def assertEqO[A: Eq](name: => Option[String], actual: A, expect: A): Unit =
    if (actual =!= expect)
      fail(s"assertEq: $name, expect: $expect, actual: $actual")

  def assertEqWithTolerance(actual: Double, expect: Double): Unit =
    _assertEqWithTolerance(None, actual, expect)

  private def _assertEqWithTolerance(
      _name: => Option[String],
      actual: Double,
      expect: Double,
      tolerance: Double = 0.001
  ): Unit = {
    val d = Math.abs(actual - expect)
    if (d > tolerance) {
      val name = _name
      val errorPrefix = name.fold("")(n => s"[$n] ")
      fail(
        s"$errorPrefix$actual ≠ $expect by $d which exceeds tolerance of $tolerance"
      )
    }
  }

  def fail(msg: String, clearStackTrace: Boolean = true): Nothing = {
    val e = new java.lang.AssertionError(msg)
    if (clearStackTrace)
      e.setStackTrace(Array.empty)
    throw e
  }

}
