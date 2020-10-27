package rocktheJvm.part2abstractMath

object Monads {
  val numbersList = List(1,2,3)
  val charsList = List('a', 'b', 'c')

  // TODO 1.1: how do you create all combinations of (number, char) ?
  val combinations: List[(Int, Char)] =
    for {
      n <- numbersList
      c <- charsList
    } yield (n, c)

  // options
  val numberOption: Option[Int] = Option(2)
  val charOption: Option[Char] = Option('d')

  // TODO 1.2: how do you create all combinations of (number, char) ?
  val combinationOption: Option[(Int, Char)] =
    for {
      n <- numberOption
      c <- charOption
    } yield (n, c)

  // cats Monad
  import cats.Monad
  import cats.instances.option._
  val optionMonad = Monad[Option]
  val anOption = optionMonad.pure(4) //Option(4) == Some(4)
  val aTransformedOption = optionMonad.flatMap(anOption)(n => if(n%3 == 0) Some(n+1) else None) // None

  // TODO 2: use a Monad[Future]
  import cats.instances.future._

  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.Future
  val monadFuture = Monad[Future]
  val aTransformedFuture: Future[Int] =
    monadFuture.flatMap(monadFuture.pure(43))(n => // require a execution context
      Future(n + 44)
    )

  // specialized API
  def getPairsList(numbers: List[Int], chars: List[Char]): List[(Int, Char)] =
    numbers.flatMap(n => chars.map(c => (n, c)))


  // extension methods - weird imports - pure, flatMap
  import cats.syntax.applicative._
  import cats.syntax.flatMap._
  import cats.syntax.functor._
  val tenOption: Option[Int] = 10.pure[Option] // implicit Monad[Option] will be use => Some(1)
  val oneOptionTransformed: Option[Int] = tenOption.flatMap(x => (x + 1).pure[Option])

  def getPairs[M[_]: Monad, A, B](ma: M[A], mb: M[B]): M[(A, B)] =
    ma.flatMap(a => mb.map(b => (a,b)))

  // My Monad
  trait MyMonad[M[_]] {
    def pure[A](value:A): M[A]
    def flatMap[A,B](ma: M[A])(f: A => M[B]): M[B]
    // TODO implements this
    def map[A,B](ma: M[A])(f: A => B): M[B] =
      flatMap(ma)(a => pure(f(a)))
  }

  // Monads extend Functors

  // TODO 4: implement a shorter version of getPairs using for-comprehensions
  def getPairsFor[M[_]: Monad, A, B](ma: M[A], mb: M[B]): M[(A,B)] =
    for {
      a <- ma
      b <- mb
    } yield (a,b)

  def main(args: Array[String]): Unit = {
    println(combinations)
  }
}
