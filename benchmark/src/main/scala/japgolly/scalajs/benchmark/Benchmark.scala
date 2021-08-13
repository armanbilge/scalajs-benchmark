package japgolly.scalajs.benchmark

import cats.data.ReaderT
import cats.effect.IO
import cats.effect.kernel.Resource
import cats.{Id, ~>}
import japgolly.scalajs.benchmark.Benchmark.SetupFn

/**
  * A benchmark. A single procedure to measure.
  *
  * @param setup Prepare the benchmark function. Setup and teardown times aren't measured.
  * @tparam P A param that alters the benchmark / data used by the benchmark.
  */
final class Benchmark[-P](val name: String, val setup: SetupFn[P], val isDisabledByDefault: Boolean) {
  override def toString = name

  // TODO Having disabledByDefault in Benchmark is a hack
  // It only makes sense for the GUI package, no?
  def setDisabledByDefault: Benchmark[P] =
    new Benchmark(name, setup, true)

  def prefix(prefix: String): Benchmark[P] =
    rename(prefix + _)

  def rename(modName: String => String): Benchmark[P] =
    rename(modName(name))

  def rename(newName: String): Benchmark[P] =
    new Benchmark(newName, setup, isDisabledByDefault)
}

object Benchmark {

  /**
    * A function that performs the part of the benchmark which should be timed.
    *
    * The timer starts immediately before running this and stop immediately on completion.
    */
  type Fn = IO[Unit]

  type ResourceIO[+A] = Resource[IO, A]
  type SetupFn[-P] = ReaderT[ResourceIO, P, IO[Unit]]
  // TODO later: type SetupFn[-I] = Setup[I, Setup[Unit, Fn]]

  /**
    * Creates a benchmark that doesn't need any external data.
    */
  def apply(name: String)(f: IO[Unit]): Benchmark[Unit] =
    new Benchmark(name, ReaderT.liftF(Resource.pure(f)), false)

  def fromFn[A](name: String)(f: A => Fn): Benchmark[A] =
    new Benchmark(name, ReaderT[Id, A, IO[Unit]](f).mapK(new (Id ~> ResourceIO) {
      def apply[A](fa: Id[A]): ResourceIO[A] = Resource.pure(fa)
    }), false)

  // def derive[A, B](name: String, f: A => Benchmark[B])(b: A => B): Benchmark[A] =
  //   new Benchmark(name, Setup.derive(f(_: A).setup)(b), false)

  // def setup[A, B](p: A => B): Builder[A, B] =
  //   Setup.simple(p).toBM

  // final class Builder[A, B](private val setup: Setup[A, B]) extends AnyVal {

  //   def apply(name: String)(f: B => Any): Benchmark[A] = {
  //     val setupFn: SetupFn[A] = setup.map(b => () => f(b))
  //     new Benchmark(name, setupFn, isDisabledByDefault = false)
  //   }

  //   def map[C](f: B => C): Builder[A, C] =
  //     new Builder(setup.map(f))
  // }

}
