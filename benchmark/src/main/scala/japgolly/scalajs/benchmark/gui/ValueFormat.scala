package japgolly.scalajs.benchmark.gui

import scala.concurrent.duration.{Duration, FiniteDuration}

/** Format for a single value.
  *
  * Eg. 32.456 sec
  */
final case class ValueFormat[-I](getDouble   : I => Option[Double],
                                 toDouble    : I => Double,
                                 toTextPretty: I => String,
                                 toTextBasic : I => String) {
  def contramap[A](f: A => I): ValueFormat[A] =
    ValueFormat(
      getDouble compose f,
      toDouble compose f,
      toTextPretty compose f,
      toTextBasic compose f)
}

object ValueFormat {

  def number(dp: Int): ValueFormat[Double] = {
    val fmt = s"%.${dp}f"
    ValueFormat(
      Some.apply,
      identity,
      GuiUtil.prettyPrintNumber(_, dp),
      fmt.format(_),
    )
  }

  def optionalNumber(dp: Int, defaultDouble: Double, defaultText: String): ValueFormat[Option[Double]] = {
    val n = number(dp)
    ValueFormat(
      identity,
      {
        case Some(d) => d
        case None    => defaultDouble
      },
      {
        case Some(d) => n toTextPretty d
        case None    => defaultText
      },
      {
        case Some(d) => n toTextBasic d
        case None    => defaultText
      })
  }

  def optionalDouble(dp: Int): ValueFormat[Option[Double]] =
    optionalNumber(
      dp            = dp,
      defaultDouble = Double.NaN,
      defaultText   = "NaN")

  def duration(getUnits: FiniteDuration => Double, dp: Int): ValueFormat[Duration] =
    optionalDouble(dp).contramap {
      case f: FiniteDuration => Some(getUnits(f))
      case _                 => None
    }

  def durationMs(getUnitsFromMs: Double => Double, dp: Int): ValueFormat[Double] =
    optionalDouble(dp).contramap(ms =>
      if (java.lang.Double.isFinite(ms))
        Some(getUnitsFromMs(ms))
      else
        None
    )

  val Integer = number(0).contramap[Int](_.toDouble)
}
