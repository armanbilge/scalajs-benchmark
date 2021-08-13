package demo.suites.cats

import cats._
import cats.free.Trampoline
import cats.implicits._
import japgolly.scalajs.benchmark._
import cats.effect.IO

object TrampolineBM {

  def evalFib(n: Int): Eval[Int] =
    if (n < 2) Eval.now(n)
    else for {
      x <- Eval.defer(evalFib(n - 1))
      y <- Eval.defer(evalFib(n - 2))
    } yield x + y

  def trampolineFib(n: Int): Trampoline[Int] =
    if (n < 2) Trampoline.done(n)
    else for {
      x <- Trampoline.defer(trampolineFib(n - 1))
      y <- Trampoline.defer(trampolineFib(n - 2))
    } yield x + y

  val suite = Suite[Int]("Trampoline")(
    Benchmark.fromFn[Int]("eval")(p => IO(evalFib(p).value)),
    Benchmark.fromFn[Int]("eval")(p => IO(trampolineFib(p).run)))

}
