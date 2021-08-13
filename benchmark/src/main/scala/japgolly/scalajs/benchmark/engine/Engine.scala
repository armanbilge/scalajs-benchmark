package japgolly.scalajs.benchmark.engine

import cats.effect.IO
import japgolly.scalajs.benchmark._
import scala.concurrent.duration.{FiniteDuration, _}
import scala.scalajs.js

sealed abstract class Event[P] {
  val progress: Progress[P]
  @inline def plan = progress.plan
}

final case class SuiteStarting     [P](progress: Progress[P])                                  extends Event[P]
final case class BenchmarkPreparing[P](progress: Progress[P], key: PlanKey[P])                 extends Event[P]
final case class BenchmarkRunning  [P](progress: Progress[P], key: PlanKey[P])                 extends Event[P]
final case class BenchmarkFinished [P](progress: Progress[P], key: PlanKey[P], result: Result) extends Event[P]
final case class SuiteFinished     [P](progress: Progress[P]/*, aborted | results, */)         extends Event[P]

final case class Progress[P](startedAt    : js.Date,
                             plan         : Plan[P],
                             runs         : Int,
                             engineOptions: EngineOptions) {

  def timestampTxt = TimeUtil.timestampStrFromJsDate(startedAt)
  def total        = plan.totalBenchmarks
  def remaining    = total - runs
}

object Progress {
  def start[P](plan: Plan[P], engineOptions: EngineOptions): Progress[P] =
    apply(new js.Date(), plan, 0, engineOptions)
}

object Engine {

  /** Runs a suite of benchmarks (asynchronously).
    *
    * You are required to handle events in order to extract any meaning.
    *
    * @return A function by which the benchmarks can be aborted.
    */
  def run[P](plan   : Plan[P],
             options: EngineOptions = EngineOptions.default)
            (onEvent: Event[P] => IO[Unit]): IO[Unit] = IO.ref(Progress.start(plan, options)).flatMap { progressRef =>

    val finish: IO[Unit] =
      progressRef.get.flatMap(p => onEvent(SuiteFinished(p)))

    val runAsync: IO[Unit] = {
      val delay = options.delay()
      val clock = options.clock

      def schedule(next: IO[Unit]): IO[Unit] =
        next.delayBy(delay)

      def msg(e: Event[P])(next: IO[Unit]): IO[Unit] =
        onEvent(e) >> schedule(next)

      def go(keys: List[PlanKey[P]]): IO[Unit] = keys match {
        case key :: next =>
          progressRef.get.flatMap { progress =>
          msg(BenchmarkPreparing(progress, key)) {

            def complete(result: Result): IO[Unit] =
              progressRef.set(progress.copy(runs = progress.runs + 1)) >> msg(BenchmarkFinished(progress, key, result))(go(next))

            val setup = key.bm.setup.run(key.param)

            setup.attempt.use {

              case Right(bm) =>

                // =====================================================================================================
                // Real benchmarking here

                msg(BenchmarkRunning(progress, key)) {
                  val bmTimedUnsafe = clock.time(bm)

                  def runIteration(s: Stats.Builder, maxTimeMs: Double): IO[Unit] = {

                    val isEnough: IO[Boolean] =
                      IO(s.totalIterationTime() >= maxTimeMs)

                    val bmRound: IO[Unit] = (for {
                      startTime <- IO.monotonic
                      delayAfter = startTime + 1.second
                      needDelay = IO.monotonic.map(_ > delayAfter)
                    } yield needDelay).flatMap { needDelay =>
                      
                      lazy val go: IO[Unit] = for {
                        t <- bmTimedUnsafe
                        _ <- IO(s.add(t))
                        isEnough <- isEnough
                        needDelay <- needDelay
                        _ <- if (!isEnough && !needDelay)
                          go
                        else
                          IO.unit
                      } yield ()

                      go

                    }


                    lazy val self: IO[Unit] = for {
                      _ <- bmRound
                      isEnough <- isEnough
                      _ <- if (isEnough)
                        self.delayBy(1.millis)
                      else
                        IO(s.endIteration())
                    } yield ()

                    self

                  }

                  def runIterations(iterations: Int, maxTime: FiniteDuration): IO[IO[Stats]] =
                    for {
                      sm <- IO(new Stats.Builder)
                      iteration = runIteration(sm, TimeUtil.toMs(maxTime))
                      _ <- (1 to iterations).foldLeft(IO.unit)((q, _) => q >> iteration)
                    } yield IO(sm.result())

                  val warmup =
                    runIterations(options.warmupIterations, options.actualWarmupIterationTime)

                  val real =
                    runIterations(options.iterations, options.iterationTime).flatten

                  (warmup >> real)
                    .attempt
                    .flatMap(complete)
                }
                // =====================================================================================================

              case Left(err) =>
                complete(Left(err))
            }
          }

          }

        case Nil =>
          finish
      }

      progressRef.get.flatMap { progress =>
        msg(SuiteStarting(progress)) {
          go(plan.keys)
        }
      }
    }

    runAsync
  }

  /** Runs a suite of benchmarks (asynchronously), printing details and results to the console.
    *
    * @return A function by which the benchmarks can be aborted.
    */
  def runToConsole[P](plan: Plan[P], options: EngineOptions = EngineOptions.default): IO[Unit] = {
    val fmt = {
      val prog  = plan.totalBenchmarks.toString.length
      val name  = plan.bms.foldLeft(0)(_ max _.name.length)
      var param = plan.params.foldLeft(0)(_ max _.toString.length)
      if (!plan.params.forall(_.toString.matches("^-?\\d+$")))
        param = -param
      s"[%${prog}d/%d] %-${name}s %${param}s : %s"
    }

    run(plan, options) {
      case SuiteStarting     (p)       => IO.println("Starting suite: " + p.plan.name)
      case BenchmarkPreparing(_, _)    => IO.unit
      case BenchmarkRunning  (_, _)    => IO.unit
      case BenchmarkFinished (p, k, r) => IO.println(fmt.format(p.runs, p.total, k.bm.name, k.param, r))
      case SuiteFinished     (p)       => IO.println("Suite completed: " + p.plan.name)
    }
  }

}
