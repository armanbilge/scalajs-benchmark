package demo.suites.scala

import cats.effect.IO
import japgolly.scalajs.benchmark._
import scala.collection.immutable._
import scala.collection.mutable

object IntSet {

  val bm = Benchmark.setup[Int, List[Int]](size =>
    // Puts it in a non-linear, deterministic order then change to disrupt hash order
    (-size to -1).toSet.iterator.map(-(_: Int)).toList
  )

  val suite = Suite("Int Set")(
    bm("immutable.Set") { is =>
      IO {
        var s = Set.empty[Int]
        for (i <- is) if (s contains i) ??? else s += i
        s
      }
    },
    bm("mutable.Set") { is =>
      IO {
        val s = mutable.Set.empty[Int]
        for (i <- is) if (s contains i) ??? else s += i
        s
      }
    },
    bm("immutable.IntMap") { is =>
      IO {
        var s = IntMap.empty[Unit]
        for (i <- is) if (s contains i) ??? else s = s.updated(i, ())
        s
      }
    },

//      bm("immutable.ListSet"){ is =>
//        var s = ListSet.empty[Int]
//        for (i <- is) if (s contains i) ??? else s += i
//        s
//      }),
    bm("immutable.HashSet") { is =>
      IO {
        var s = HashSet.empty[Int]
        for (i <- is) if (s contains i) ??? else s += i
        s
      }
    },
    bm("mutable.HashSet") { is =>
      IO {
        val s = mutable.HashSet.empty[Int]
        for (i <- is) if (s contains i) ??? else s += i
        s
      }
    },
    bm("immutable.TreeSet") { is =>
      IO {
        var s = TreeSet.empty[Int]
        for (i <- is) if (s contains i) ??? else s += i
        s
      }
    },
    bm("mutable.TreeSet") { is =>
      IO {
        val s = mutable.TreeSet.empty[Int]
        for (i <- is) if (s contains i) ??? else s += i
        s
      }
    },
    bm("immutable.BitSet") { is =>
      IO {
        var s = BitSet.empty
        for (i <- is) if (s contains i) ??? else s += i
        s
      }
    },
    bm("mutable.BitSet (contains and +=)") { is =>
      IO {
        val s = mutable.BitSet.empty
        for (i <- is) if (s contains i) ??? else s += i
        s
      }
    },
    bm("mutable.BitSet (add)") { is =>
      IO {
        val s = mutable.BitSet.empty
        for (i <- is) if (!s.add(i)) ???
        s
      }
    },

    // ===============================================================================================================
    // Add then check by ref
    bm("immutable.Set (eq)") { is =>
      IO {
        var s = Set.empty[Int]
        for (i <- is) {
          val b = s; s += i; if (b eq s) ???
        }
        s
      }
    },

//      bm("immutable.ListSet (eq)"){ is =>
//        var s = ListSet.empty[Int]
//        for (i <- is) {
//          val b = s; s += i; if (b eq s) ???
//        }
//        s
//      }),
    bm("immutable.HashSet (eq)") { is =>
      IO {
        var s = HashSet.empty[Int]
        for (i <- is) {
          val b = s; s += i; if (b eq s) ???
        }
        s
      }
    },
    bm("immutable.BitSet (eq)") { is =>
      IO {
        var s = BitSet.empty
        for (i <- is) {
          val b = s; s += i; if (b eq s) ???
        }
        s
      }
    }
  )

}
