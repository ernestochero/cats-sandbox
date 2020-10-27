package part01.chapter05

object MonadTransformerCats extends App {
  import cats.Monad
  import cats.syntax.applicative._
  def compose[M1[_]: Monad, M2[_]: Monad] = {
    type Composed[A] = M1[M2[A]]
    new Monad[Composed] {
      override def pure[A](x: A): Composed[A] = x.pure[M2].pure[M1]
      override def flatMap[A, B](fa: Composed[A])(f: A => Composed[B]): Composed[B] = ???
      override def tailRecM[A, B](a: A)(f: A => Composed[Either[A, B]]): Composed[B] = ???
    }
  }
  import cats.data.OptionT
  type ListOption[A] = OptionT[List, A]
  import cats.instances.list._
  val result1: ListOption[Int] = OptionT(List(Option(10)))
  val result2: ListOption[Int] = 32.pure[ListOption] // I like it!
  val result3: ListOption[Int] = for {
    x <- result1
    y <- result2
  } yield x + y
  println(result3)

  import cats.instances.either._
  type ErrorOr[A] = Either[String, A]
  type ErrorOrOption[A] = OptionT[ErrorOr, A]
  val a = 10.pure[ErrorOrOption]
  val b = 32.pure[ErrorOrOption]
  val c = a.flatMap(x => b.map(y => x + y))

  import cats.data.EitherT
  import cats.instances.future._
  import scala.concurrent.Future
  import scala.concurrent.Await
  import scala.concurrent.duration._
  import scala.concurrent.ExecutionContext.Implicits.global
  type FutureEither[A] = EitherT[Future, String, A]
  type FutureEitherOption[A] = OptionT[FutureEither, A]
  val futureEitherOr: FutureEitherOption[Int] =
    for {
      a <- 10.pure[FutureEitherOption]
      b <- 32.pure[FutureEitherOption]
    } yield a + b
  val intermediate = futureEitherOr.value
  val stack = intermediate.value
  println(Await.result(stack, 1.second))
  val errorStack1 = OptionT[ErrorOr, Int](Right(Some(10)))
  val errorStack2 = 32.pure[ErrorOrOption]
  println(errorStack1.value)
  println(errorStack2.value.map(_.getOrElse(-1)))


  sealed abstract class HttpError
  final case class NotFound(item: String) extends HttpError
  final case class BadRequest(msg: String) extends HttpError

  type FutureEitherHttp[A] = EitherT[Future, HttpError, A]

  import cats.data.Writer
  type Logged[A] = Writer[List[String], A]
  // Methods generally return untransformed stacks:
  def parseNumber(str: String): Logged[Option[Int]] =
    util.Try(str.toInt).toOption match {
      case Some(num) => Writer(List(s"Read $str"), Some(num))
      case None => Writer(List(s"Failed on $str"), None)
    }

  // Consumers use monad transformers locally to simplify composition:
  def addAll(a: String, b: String, c: String): Logged[Option[Int]] = {
    import cats.data.OptionT
    val result = for {
      a <- OptionT(parseNumber(a))
      b <- OptionT(parseNumber(b))
      c <- OptionT(parseNumber(c))
    } yield a + b + c
    result.value
  }

  val r1 = addAll("1", "2", "3")
  println(r1)
  val r2 = addAll("1", "a", "3")
  println(r2)

  // exercise about monad transformers
  val powerLevels = Map(
    "Jazz" -> 6,
    "Bumblebee" -> 8,
    "Hot Rod" -> 10
  )
  type Response[A] = EitherT[Future, String, A]
  def getPowerLevel(ally: String): Response[Int] =
    powerLevels.get(ally) match {
      case Some(x) => EitherT.right(Future(x))
      case None => EitherT.left(Future(s"$ally unreachable"))
    }
  import cats.syntax.either._
  def getPowerLevel2(ally: String): Response[Int] = {
    val r = powerLevels.get(ally) match {
      case Some(x) => x.asRight
      case None => s"$ally unreachable".asLeft
    }
    EitherT(Future(r))
  }
  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] = {
    for {
      pl1 <- getPowerLevel(ally1)
      pl2 <- getPowerLevel(ally2)
    } yield (pl1 + pl2) > 15
  }

  def tacticalReport(ally1: String, ally2: String): String =
    Await.result(canSpecialMove(ally1, ally2).value, 1.second) match {
      case Right(true) =>
        s"$ally1 and $ally2 are ready to roll out!"
      case Right(false) =>
        s"$ally1 and $ally2 need a recharge."
      case Left(msg) => s"Comms error: $msg"
    }


}
