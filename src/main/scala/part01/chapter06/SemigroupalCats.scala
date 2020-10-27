package part01.chapter06

object SemigroupalCats extends App {
  import cats.Semigroupal
  import  cats.instances.option._
  Semigroupal[Option].product(Some(123), Some("123")) // Some((123, "123"))
  Semigroupal[Option].product(None, Some("abc")) // Option[Tuple2[Nothing, String]] = None
  Semigroupal[Option].product(Some("abc"), None) // Option[Tuple2[String, Nothing]] = None

  Semigroupal.tuple3(Option(1), Option(2), Option(3)) // Option[(Int, Int, Int)] = Some((1, 2, 3))
  Semigroupal.tuple3(Option(1), Option(2), Option.empty[Int]) // // res5: Option[(Int, Int, Int)] = None

  Semigroupal.map3(Option(1), Option(2), Option(3))(_ + _ + _) //  Option[Int] = Some(6)
  Semigroupal.map2(Option(1), Option.empty[Int])(_ + _) // Option[Int] = None

  import cats.syntax.apply._ // for tupled and mapN
  (Option(123), Option("abc")).tupled
  (Option(1), Option(2), Option(3)).mapN(_ + _ + _)

  final case class MyCat(name: String,  born: Int, color: String)
  object MyCat {
    def validateBorn(born: Int): Option[Int] =
      if(born > 1978) Some(born)
      else None
  }

  val garfield = (Option("Garfield"), Option(1978).flatMap(MyCat.validateBorn), Option("Orange & Black"))
    .mapN(MyCat.apply)

  import cats.Monoid
  import cats.instances.int._
  import cats.instances.invariant._
  import cats.instances.list._
  import cats.instances.string._

  final case class Cat(name: String, yearOfBirth: Int, favouriteFoods: List[String])
  val tupleToCat: (String, Int, List[String]) => Cat = Cat.apply
  val catToTuple: Cat => (String, Int, List[String]) = cat => (cat.name, cat.yearOfBirth, cat.favouriteFoods)
  implicit val catMonoid: Monoid[Cat] = (
    Monoid[String], Monoid[Int], Monoid[List[String]]
    ).imapN(tupleToCat)(catToTuple)

  import cats.syntax.semigroup._
  val garfield2 = Cat("Garfield", 1978, List("Lasagne"))
  val heathcliff = Cat("Heathcliff", 1988, List("Junk Food"))
  garfield2 |+| heathcliff

  import cats.instances.future._
  import scala.concurrent._
  import scala.concurrent.duration._
  import scala.concurrent.ExecutionContext.Implicits.global

  val futurePair = Semigroupal[Future]
    .product(Future("Hello"), Future(123))
  Await.result(futurePair, 1.second)

  val futureCat = (Future("Garfield"), Future(1978), Future(List("Lasagne"))).mapN(Cat.apply)
  Await.result(futureCat, 1.second)
  Semigroupal[List].product(List(1, 2), List(3, 4)) // List[(Int, Int)] = List((1, 3), (1, 4), (2, 3), (2, 4))


  // import cats.syntax.parallel._
  type ErrorOr[A] = Either[Vector[String], A]
  val success1: ErrorOr[Int] = Right(1)
  val success2: ErrorOr[Int] = Right(2)
  val error1: ErrorOr[Int] = Left(Vector("Error 1"))
  val error2: ErrorOr[Int] = Left(Vector("Error 2"))
  val addTwo = (x: Int, y: Int) => x + y
  /*  (error1, error2).parMapN(addTwo)
    (success1, success2).parMapN(addTwo)
    (success1, error1).parMapN(addTwo)*/

  import cats.arrow.FunctionK
  object OptionToList extends FunctionK[Option, List] {
    override def apply[A](fa: Option[A]): List[A] =
      fa.fold(List.empty[A])(List(_))
  }
  println { OptionToList(Some(1)) }
  println { OptionToList(None) }
}