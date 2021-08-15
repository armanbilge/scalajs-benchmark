package japgolly.scalajs.benchmark.engine

import scala.scalajs.js

class Blackhole {
  var x = false
  var ar: AnyRef = ""
  var consumedCPU: js.BigInt = js.BigInt(System.nanoTime().toString)

  final def consume(obj: AnyRef): Unit =
    x ^= obj eq ar

  final def consume(objs: Array[AnyRef]): Unit =
    x ^= objs eq ar

  final def consume(b: Byte): Unit =
    x ^= b == 0

  final def consume(bool: Boolean): Unit =
    x ^= bool

  final def consume(c: Char): Unit =
    x ^= c == 0

  final def consume(s: Short): Unit =
    x ^= s == 0

  final def consume(i: Int): Unit =
    x ^= i == 0

  final def consume(l: Long): Unit =
    x ^= l.toInt == 0

  final def consume(f: Float): Unit =
    x ^= f == 0

  final def consume(d: Double): Unit =
    x ^= d == 0

  final def consumeA[A](a: A): Unit =
    consume(a :: Nil)

  final def consumeCPU(tokens: Int): Unit = {
    var t = consumedCPU

    var i = tokens
    while (i > 0) {
      t += (t * js.BigInt(0x5deece66dL.toString) + js.BigInt(0xbL.toString) + js
        .BigInt(i)) & js.BigInt(0xffffffffffffL.toString)
      i -= 1
    }

    if (t == js.BigInt(42))
      consumedCPU += t
  }
}
