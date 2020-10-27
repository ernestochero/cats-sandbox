package rocktheJvm.part4TypeClasses

object HandlingErrors {
  import cats.MonadError
  import cats.instances.either._
  type ErrorOr[A] = Either[String, A]
  val monadErrorEither: MonadError[ErrorOr, String] =
    MonadError[ErrorOr, String]
  val success: ErrorOr[Int] =
    monadErrorEither.pure(32) // Either[String, Int] = Right(32)
  val failure: ErrorOr[Int] = monadErrorEither.raiseError[Int](
    "something wrong"
  ) // Either[String, Int] = Right(32)
  val handledError: ErrorOr[Int] =
    monadErrorEither.handleError(failure) {
      case "Badness" => 33
      case _         => 89
    }

  val handleErrorWith: ErrorOr[Int] =
    monadErrorEither.handleErrorWith(failure) {
      case "Badness" => monadErrorEither.pure(33)
      case _         => Left("something else")
    }

  // applicatives => ApplicativeError
  import cats.data.Validated
  import cats.ApplicativeError
  import cats.instances.list._
  type ErrorsOr[T] = Validated[List[String], T]

  val applicativeErrorVal: ApplicativeError[ErrorsOr, List[String]] =
    ApplicativeError[ErrorsOr, List[String]]

  def main(args: Array[String]): Unit = {}
}
