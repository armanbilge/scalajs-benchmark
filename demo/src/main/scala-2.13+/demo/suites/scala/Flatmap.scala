package demo.suites.scala

import cats.effect.IO
import japgolly.scalajs.benchmark._
import scala.collection.immutable._

object Flatmap {

  case class Params(size1: Int, size2: Int) {
    override val toString = s"$size1 * $size2"
  }

  type A = Int
  val a: A = 123

  private def makeBmFn(as: => IterableOnce[A]): Benchmark.Fn =
    IO(() => as.iterator.foreach(_ => ()))

  private def makeTravBm[C[x] <: Iterable[x]](name: String, fill: (Int, A) => C[A]): Benchmark[Params] =
    Benchmark.fromFn[Params](name) { p =>
      val flatOut = fill(p.size2, a)
      val input = fill(p.size1, a)
      makeBmFn(input.flatMap(_ => flatOut))
    }

  private def makeIterBm[C[x] <: Iterable[x]](name: String, fill: (Int, A) => C[A]): Benchmark[Params] =
    Benchmark.fromFn[Params](name + ".iterator") { p =>
      val flatOut = fill(p.size2, a)
      val input = fill(p.size1, a)
      makeBmFn(input.iterator.flatMap(_ => flatOut))
    }

  def bm[C[x] <: Iterable[x]](name: String, fill: (Int, A) => C[A]): Vector[Benchmark[Params]] =
    Vector(makeTravBm(name, fill), makeIterBm(name, fill))

  val suite = {
    var bms = Vector.empty[Benchmark[Params]]
    bms ++= bm("ArraySeq", ArraySeq.fill(_)(_))
    bms ++= bm("LazyList", LazyList.fill(_)(_))
    bms ++= bm("List", List.fill(_)(_))
    bms ++= bm("Queue", Queue.fill(_)(_))
    bms ++= bm("Vector", Vector.fill(_)(_))
    Suite("FlatMap")(bms: _*)
  }

}
