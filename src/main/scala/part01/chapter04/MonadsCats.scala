package part01.chapter04

object MonadsCats extends App {
  trait MyMonad[F[_]] {
    def pure[A](value: A): F[A]
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
    def map[A,B](fa: F[A])(f: A => B): F[B] =
      flatMap(fa)(a => pure(f(a)))
  }

  import cats.Monad
  import cats.instances.option._
  import cats.instances.list._

  val opt1 = Monad[Option].pure(10)
  val opt2 = Monad[Option].flatMap(opt1)(a => Some(a + 22))
  val opt3 = Monad[Option].flatMap(opt2)(a => Some(100 * a))

  val list1 = Monad[List].pure(3)
  val list2 = Monad[List].flatMap(list1)(a => List(a, a * 10))

  val listT = (1 to 10).toList
  val f = (v: Int) => List(v, v * 10)
  // hard way
  def g(l: List[Int]): List[Int] = {
    l match {
      case x::xs =>
        val s: List[Int] = Monad[List].flatMap(List(x))(f)
        s ++ g(xs)
      case Nil => List.empty[Int]
    }
  }
  // easy way
  listT.flatMap(f)

  import cats.syntax.functor._
  import cats.syntax.flatMap._

  def sumSquare[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] =
    for {
      x <- a
      y <- b
    } yield x*x + y*y
  sumSquare(Option(3), Option(4))
  sumSquare(List(1,2,3), List(4, 5))

  import cats.Id
  // consider that : type Id[A] = A
  def pure[A](a: A): Id[A] = a
  def map[A,B](fa: Id[A])(f: A => B): Id[B] = f(fa)
  def flapMap[A,B](fa: Id[A])(f: A => Id[B]): Id[B] = f(fa)


  println(sumSquare(Monad[Id].pure(3),Monad[Id].pure(4)))
  println(sumSquare(3: Id[Int],4: Id[Int]))


  import cats.syntax.either._
  val a = 3.asRight[String]
  val b = 4.asRight[Int]
  val c = 5.asRight[Int]
  println {
    for {
      x <- a
      y <- b
      z <- c
    } yield x + y + z
  }
  // this method stop when a negative number is
  // found and return the sum until this step
  def countPositive(nums: List[Int]) =
    nums.foldLeft(0.asRight[Int]){ (acc, num) =>
      if(num > 0) acc.map(_ + num)
      else acc.flatMap(_.asLeft[Int])
    }
  def printFormat(either: Either[Int, Int]) = either match {
    case Right(v) =>
      println(s"everything has gone well sum => ${v}")
    case Left(x) =>
      println(s"everything has not gone well sum => ${x}")
  }
  printFormat(countPositive(List(1,10,6,1 ,20)))

  // catching exceptions with either
  Either.catchOnly[NumberFormatException]("hello".toInt)
  Either.catchNonFatal(sys.error("Badness"))
  val eitherInt = (-2).asRight[String].ensureOr(i => s"${i} Must be non-negative")(_ > 0)
  val test = 5.asLeft[String].swap
  println(test.map(c => s"Hey ${c}"))


  // Error handling
  type Result[A] = Either[Throwable, A]
  val result: Result[Int] =  10.asRight[Throwable]
  println(result)

  // own wrapper
  object wrapper {
    sealed trait LoginError extends Product with Serializable
    final case class UserNotFound(username: String) extends LoginError
    final case class PasswordIncorrect(username: String) extends LoginError
    case object UnexpectedError extends LoginError
  }
  import wrapper._
  case class User(username: String, password: String)
  type LoginResult = Either[LoginError, User]

  def handlingError(error: LoginError): Unit =
    error match {
      case UserNotFound(username) =>
          println(s"User not found: $username")
      case PasswordIncorrect(username) =>
          println(s"Password Incorrect $username")
      case UnexpectedError =>
          println("Unexpected error")
    }
  val result1: LoginResult = User("ernesto", "12340rd").asRight
  val result2: LoginResult = UserNotFound("ernesto").asLeft
  result1.fold(handlingError, println)
  result2.fold(handlingError, println)

  import cats.MonadError
  import cats.instances.either._
  type ErrorOr[A] = Either[String, A]
  val monadError = MonadError[ErrorOr, String]
  monadError.pure(42)
  monadError.raiseError("Badness")
  import scala.util.Try
  import cats.instances.try_._
  def validateAdult[F[_]](age: Int)(implicit me: MonadError[F, Throwable]): F[Int] = {
    if(age > 18) me.pure(age)
    else me.raiseError(new Throwable("Age must be greater than or equal to 18"))
  }
  validateAdult[Try](20)
  validateAdult[Try](8)

  type ExceptionOr[A] = Either[Throwable, A]
  println(validateAdult[ExceptionOr](-1))

}
