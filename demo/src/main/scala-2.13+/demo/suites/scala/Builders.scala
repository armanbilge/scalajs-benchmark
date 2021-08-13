package demo.suites.scala

import cats.effect.IO
import japgolly.scalajs.benchmark._
import scala.collection.immutable._

object Builders {

  type A = AnyRef

  def newA: A = new AnyRef

  val bm = Benchmark.setup[Int, List[A]](size => List.fill[A](size)(newA))

  val suite = Suite("Collection Building")(
    bm("Array builder") { is =>
      IO {
        val b = Array.newBuilder[A]
        for (i <- is) b += i
        b.result()
      }
    }.setDisabledByDefault,
    bm("ArraySeq var (append)") { is =>
      IO {
        var s = ArraySeq.empty[A]
        for (i <- is) s = s :+ i
        s
      }
    }.setDisabledByDefault,
    bm("ArraySeq var (prepend)") { is =>
      IO {
        var s = ArraySeq.empty[A]
        for (i <- is) s = i +: s
        s
      }
    }.setDisabledByDefault,
    bm("ArraySeq builder") { is =>
      IO {
        val b = ArraySeq.newBuilder[A]
        for (i <- is) b += i
        b.result()
      }
    },
    bm("LazyList var (append)") { is =>
      IO {
        var s = LazyList.empty[A]
        for (i <- is) s = i #:: s
        s
      }
    },
    bm("LazyList var (prepend)") { is =>
      IO {
        var s = LazyList.empty[A]
        for (i <- is) s = i #:: s
        s
      }
    },
    bm("LazyList builder") { is =>
      IO {
        val b = LazyList.newBuilder[A]
        for (i <- is) b += i
        b.result()
      }
    }.setDisabledByDefault,
    bm("List var (append)") { is =>
      IO {
        var s = List.empty[A]
        for (i <- is) s = s :+ i
        s
      }
    }.setDisabledByDefault,
    bm("List var (prepend)") { is =>
      IO {
        var s = List.empty[A]
        for (i <- is) s = i :: s
        s
      }
    },
    bm("List var, prepend & reverse") { is =>
      IO {
        var s = List.empty[A]
        for (i <- is) s = i :: s
        s.reverse
      }
    },
    bm("List builder") { is =>
      IO {
        val b = List.newBuilder[A]
        for (i <- is) b += i
        b.result()
      }
    },
    bm("Seq var (append)") { is =>
      IO {
        var s = Seq.empty[A]
        for (i <- is) s = s :+ i
        s
      }
    }.setDisabledByDefault,
    bm("Seq var (prepend)") { is =>
      IO {
        var s = Seq.empty[A]
        for (i <- is) s = i +: s
        s
      }
    }.setDisabledByDefault,
    bm("Seq builder") { is =>
      IO {
        val b = Seq.newBuilder[A]
        for (i <- is) b += i
        b.result()
      }
    },
    bm("Vector var (append)") { is =>
      IO {
        var s = Vector.empty[A]
        for (i <- is) s = s :+ i
        s
      }
    },
    bm("Vector var (prepend)") { is =>
      IO {
        var s = Vector.empty[A]
        for (i <- is) s = i +: s
        s
      }
    },
    bm("Vector builder") { is =>
      IO {
        val b = Vector.newBuilder[A]
        for (i <- is) b += i
        b.result()
      }
    }
  )

}
