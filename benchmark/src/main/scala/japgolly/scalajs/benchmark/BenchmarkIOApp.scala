package japgolly.scalajs.benchmark

import cats.effect.IOApp
import cats.effect.{ExitCode, IO}
import cats.syntax.all._
import japgolly.scalajs.benchmark.engine.Engine
import japgolly.scalajs.benchmark.engine.EngineOptions
import japgolly.scalajs.benchmark.engine._
import japgolly.scalajs.benchmark.gui.BmResultFormat

abstract class BenchmarkIOApp[P] extends IOApp {

  final def run(args: List[String]): IO[ExitCode] = plans
    .traverse_ { plan =>
      val fmt = {
        val prog = plan.totalBenchmarks.toString.length
        val name = plan.bms.foldLeft(0)(_ max _.name.length)
        var param = plan.params.foldLeft(0)(_ max _.toString.length)
        if (!plan.params.forall(_.toString.matches("^-?\\d+$")))
          param = -param
        s"[%${prog}d/%d] %-${name}s %${param}s : %s"
      }

      val bmFormat = BmResultFormat.OpsPerSec3

      Engine
        .run(plan, options) {
          case SuiteStarting(p) => IO.println("Starting suite: " + p.plan.name)
          case BenchmarkPreparing(_, _) => IO.unit
          case BenchmarkRunning(_, _)   => IO.unit
          case BenchmarkFinished(p, k, Right(stats)) =>
            val score = bmFormat.score.toTextPretty(stats)
            val error = bmFormat.scoreError.toTextPretty(stats)
            val result = s"${score} ± ${error} ${bmFormat.header}"
            IO.println(fmt.format(p.runs, p.total, k.bm.name, k.param, result))
          case BenchmarkFinished(p, k, Left(ex)) =>
            IO.println(fmt.format(p.runs, p.total, k.bm.name, k.param, ex))
          case SuiteFinished(p) => IO.println("Suite completed: " + p.plan.name)
        }
    }
    .attempt
    .map {
      case Right(_) => ExitCode.Success
      case Left(_)  => ExitCode.Error
    }

  def options: EngineOptions = EngineOptions.default
  def plans: Seq[Plan[_]]

}
