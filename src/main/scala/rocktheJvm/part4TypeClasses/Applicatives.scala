package rocktheJvm.part4TypeClasses

object Applicatives {
  // Applicatives = Functors + the pure method
  import cats.Applicative
  import cats.instances.list._
  val listApplicative: Applicative[List] = Applicative[List]
  val aList: List[Int] = listApplicative.pure(2) // List(2)

  import cats.instances.option._
  val aOption: Option[Int] = Applicative[Option].pure(10) // Some(10)

  // pure extension method
  import cats.syntax.applicative._
  val bOption: Option[Int] = 20.pure[Option]

  // Monads extends from applicative
  // Applicatives extend from Functors
  import cats.data.Validated
  type ErrorsOr[T] = Validated[List[String], T]
  val aValidValue: ErrorsOr[Int] = Validated.valid(20)
  val aModifiedValue: ErrorsOr[Int] = aValidValue.map(_ + 1)
  val validateApplicative: Applicative[ErrorsOr] = Applicative[ErrorsOr]
  val bValidValue: ErrorsOr[Int] = validateApplicative.pure(20)

  // TODO: thought experiment
  //applicative.tuple2(wa, wb)
  def productWithApplicatives[W[_], A, B](wa: W[A], wb: W[B])(implicit
      applicative: Applicative[W]
  ): W[(A, B)] = {
    val functionWrapper: W[B => (A, B)] =
      applicative.map(wa)(a => (b: B) => (a, b))
    applicative.ap(functionWrapper)(wb)
  }

  import cats.FlatMap

  def main(args: Array[String]): Unit = {}
}
