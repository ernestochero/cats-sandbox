package rocktheJvm.part4TypeClasses

import cats.Semigroup

object Semigroupals {
  import cats.Semigroupal
  // def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  import cats.instances.option._
  val aTupleOption: Option[(Int, String)] =
    Semigroupal[Option].product(Some(123), Some("a String"))
  // after print it : Some(123, "a String")
  val aNoneTuple = Semigroupal[Option].product(Some(123), None) // None

  import cats.Monad
  import cats.syntax.functor._
  import cats.syntax.flatMap._
  def productWithMonads[F[_]: Monad, A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    for {
      a <- fa
      b <- fb
    } yield (a, b)

  import cats.data.Validated
  import cats.instances.list._
  type ErrorsOr[T] = Validated[List[String], T]
  val invalidsCombination = Semigroupal[ErrorsOr].product(
    Validated.invalid(List("Something wrong", "Something else wrong")),
    Validated.invalid(List("This can't be right"))
  )

  import cats.instances.either._ // implicit Monad[Either]
  type EitherErrorsOr[T] = Either[List[String], T]
  val eitherCombination =
    Semigroupal[EitherErrorsOr].product( // in terms of map/flatMap
      Left(List("Something wrong", "Something else wrong")),
      Left(List("This can't be right"))
    )

  // TODO 2: define a Semigroupal[List] which does a zip
  val zipListSemigroupal: Semigroupal[List] = new Semigroupal[List] {
    override def product[A, B](listA: List[A], listB: List[B]): List[(A, B)] =
      listA.zip(listB)
  }

  def main(args: Array[String]): Unit = {
    println(aNoneTuple)
    println(
      invalidsCombination
    ) // Invalid(List(Something wrong, Something else wrong, This can't be right))
    println(
      eitherCombination
    ) // Left(List(Something wrong, Something else wrong))

    println(
      zipListSemigroupal
        .product(List(1, 2), List("a", "b")) // List((1,a), (2,b))
    )

  }
}
