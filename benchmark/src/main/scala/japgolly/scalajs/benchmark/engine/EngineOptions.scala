package japgolly.scalajs.benchmark.engine

import java.util.concurrent.TimeUnit
import scala.concurrent.duration._

/**
  * @param warmupIterationTime None means use [[iterationTime]]
  */
final case class EngineOptions(clock              : Clock,
                               initialDelay       : FiniteDuration,
                               delay              : () => FiniteDuration,
                               warmupIterations   : Int,
                               warmupIterationTime: Option[FiniteDuration],
                               iterations         : Int,
                               iterationTime      : FiniteDuration,
                              ) {

  val actualWarmupIterationTime: FiniteDuration =
    warmupIterationTime.getOrElse(iterationTime)

  val estimatedTimePerBM: FiniteDuration =
    EngineOptions.estimatedOverheadPerBm +
      warmupIterations * actualWarmupIterationTime +
      iterationTime * iterations

  val estimatedMsPerBM: Double =
    TimeUtil.toMs(estimatedTimePerBM)
}

object EngineOptions {

  lazy val default: EngineOptions =
    apply(
      clock               = Clock.Default,
      initialDelay        = 4.millis,
      delay               = defaultDelay,
      warmupIterationTime = None,
      warmupIterations    = 1,
      iterations          = 10,
      iterationTime       = 2.seconds,
    )

  // Ensure benchmarks don't start before chart animation finishes
  private def defaultDelay: () => FiniteDuration = () => {
    FiniteDuration(0, TimeUnit.MILLISECONDS)
  }

  private val estimatedOverheadPerBm =
    FiniteDuration(2000, TimeUnit.MILLISECONDS)

}
