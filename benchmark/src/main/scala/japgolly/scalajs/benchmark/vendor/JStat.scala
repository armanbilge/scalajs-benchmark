package japgolly.scalajs.benchmark.vendor

import scala.scalajs.js

object JStat extends js.Object {

  private[this] val jstat = js.Dynamic.global.require("jstat")

  object studentt extends js.Object {
    def inv(p: Double, df: Int): Double = jstat.studentt.inv(p, df).asInstanceOf[Double]
  }

}
