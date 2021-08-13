package demo.suites.scala

import cats.effect.IO
import japgolly.scalajs.benchmark._
import scala.collection.immutable._

object VectorIndex {

  case class Cfg(vectorSize: Int, index: Int) {
    override def toString = s"$vectorSize($index)"
  }

  case class Data(vector: Vector[Int], index: Int)

  val bm = Benchmark.setup[Cfg, Data](cfg =>
    Data(Vector.fill(cfg.vectorSize)(0), cfg.index)
  )

  @inline private def ensureType(o: Option[Int]) = o

  val suite = Suite("Vector index")(
    bm("try/catch")(d =>
      IO {
        ensureType(
          try Some(d.vector(d.index))
          catch {
            case _: IndexOutOfBoundsException => None
          }
        )
      }
    ),
    bm("check length")(d =>
      IO {
        ensureType(
          if (d.index >= 0 && d.index < d.vector.length)
            Some(d.vector(d.index))
          else
            None
        )
      }
    ),
    bm("lift")(d =>
      IO {
        ensureType(
          d.vector.lift(d.index)
        )
      }
    )
  )

}
