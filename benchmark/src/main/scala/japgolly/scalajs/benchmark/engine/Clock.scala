package japgolly.scalajs.benchmark.engine


import cats.effect.IO

trait Clock {
  /** @return time `f` took to complete, in milliseconds */
  def time(f: IO[Unit]): IO[Double]
}

object Clock {
  val Default: Clock = f => for {
    start <- IO.realTime
    _ <- f
    end <- IO.realTime
  } yield (end - start).toMillis.toDouble
}
