package japgolly.scalajs.benchmark.engine

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.{FiniteDuration, TimeUnit}
import utest._
import japgolly.scalajs.benchmark.TestUtil

object TimeUtilTest extends TestSuite {

  override def tests = Tests {
    "getUnitsFromMs" - {
      def test(t: TimeUnit): Unit = {
        val oneAsMs = TimeUtil.toMs(FiniteDuration(1, t))
        TestUtil.assertEq(TimeUtil.getUnitsFromMs(t)(oneAsMs), 1d)
      }

      "NANOSECONDS"  - test(TimeUnit.NANOSECONDS)
      "MICROSECONDS" - test(TimeUnit.MICROSECONDS)
      "MILLISECONDS" - test(TimeUnit.MILLISECONDS)
      "SECONDS"      - test(TimeUnit.SECONDS)
      "MINUTES"      - test(TimeUnit.MINUTES)
      "HOURS"        - test(TimeUnit.HOURS)
      "DAYS"         - test(TimeUnit.DAYS)
    }
  }
}
