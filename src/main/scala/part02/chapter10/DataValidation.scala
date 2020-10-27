package part02.chapter10

object DataValidation {
  import cats.Monad
  sealed trait Impl[F[_], A, B, C] {
    val aToB: A => F[B]
    val bToC: B => F[C]
    def example(a: A)(implicit f: Monad[F]): F[C] =
      f.flatMap(f.flatMap(f.pure(a))(aToB))(bToC)
  }

  import cats.data.Kleisli
  import cats.instances.list._ // for Monad
  val step1: Kleisli[List, Int, Int] =
    Kleisli(x => List(x + 1, x - 1))

  val step2: Kleisli[List, Int, Int] =
    Kleisli(x => List(x, -x))

  val step3: Kleisli[List, Int, Int] =
    Kleisli(x => List(x * 2, x / 2))

  val pipeline: Kleisli[List, Int, Int] = step1 andThen step2 andThen step3

  def main(args: Array[String]): Unit = {
    println(pipeline.run(20))
  }

}
