package demo.suites.scala

import cats.effect.IO
import japgolly.scalajs.benchmark._
import scala.collection.immutable._

object SetBuilding {

  case class Params(uniq: Int, dups: Int)

  type A = Int
  val bm = Benchmark.setup[Params, List[A]] { p =>
    ((0 until p.uniq).iterator ++ Array.fill(p.dups)(0)).toList
  }

  val suite = Suite("Set building")(
    bm("Set.fold")(as =>
      IO {
        as.foldLeft(Set.empty[A])(_ + _)
      }
    ),
    bm("Set.newBuilder") { as =>
      IO {
        val b = Set.newBuilder[A]
        as foreach (b += _)
        b.result()
      }
    },
    bm("HashSet.fold")(as => IO(as.foldLeft(HashSet.empty[A])(_ + _))),
    bm("HashSet.newBuilder") { as =>
      IO {
        val b = HashSet.newBuilder[A]
        as foreach (b += _)
        b.result()
      }
    },
    bm("TreeSet.fold")(as => IO(as.foldLeft(TreeSet.empty[A])(_ + _))),
    bm("TreeSet.newBuilder") { as =>
      IO {
        val b = TreeSet.newBuilder[A]
        as foreach (b += _)
        b.result()
      }
    },
    bm("ListSet.fold")(as =>
      IO(as.foldLeft(ListSet.empty[A])(_ + _))
    ).setDisabledByDefault,
    bm("ListSet.newBuilder") { as =>
      IO {
        val b = ListSet.newBuilder[A]
        as foreach (b += _)
        b.result()
      }
    }
  )

}
