package demo.suites.scalajs

import cats.effect.IO
import japgolly.scalajs.benchmark._

object Allocation {

  final case class Config(classes2: Int)


  // ===================================================================================================================

  final case class Class2(arg1: String, arg2: Config)

  type Prep = () => Array[_]

  val bm = Benchmark.setup[Config, Prep] { cfg =>
    val array = new Array[Class2](cfg.classes2)
    val str = "I'm a string"
    () => {
      var i = array.length
      while (i > 0) {
        i -= 1
        array(i) = Class2(str, cfg)
      }
      array
    }
  }

  val suite = Suite("Allocations")(

    bm("Classes")(prep => IO(prep()).void)

  )

}
