package japgolly.scalajs.benchmark

import cats.effect.IOApp
import cats.effect.{ExitCode, IO}
import japgolly.scalajs.benchmark.engine.Engine

class TestBenchmark extends IOApp {

  val suite = Suite[Unit]("Example Benchmarks")(

      // Benchmark #1
      Benchmark("foreach") {
        IO {
          var s = Set.empty[Int]
          (1 to 100) foreach (s += _)
          s
        }
      },

      // Benchmark #2
      Benchmark("fold") {
        IO {
          (1 to 100).foldLeft(Set.empty[Int])(_ + _)
        }
      }
    )

  override def run(args: List[String]): IO[ExitCode] =
    Engine.runToConsole(Plan(suite, Vector(()))).as(ExitCode.Success)

}
