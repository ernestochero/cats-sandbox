package part01.chapter07

object TransverseCats extends App {
  import scala.concurrent._
  import scala.concurrent.duration._
  import scala.concurrent.ExecutionContext.Implicits.global
  val hostnames = List(
    "alpha.example.com", "beta.example.com", "gama.demo.com"
  )
  def getUptime(hostname: String): Future[Int] =
    Future(hostname.length * 60)

  val allUptimes: Future[List[Int]] =
    hostnames.foldLeft(Future(List.empty[Int])) {
      (accum, host) =>
        val uptime = getUptime(host)
        for {
          acc <- accum
          upt <- uptime
        } yield acc :+ upt
    }
  Await.result(allUptimes, 1.second) // List[Int] = List(1020, 960, 840)

  val allUptimes2: Future[List[Int]] =
    Future.traverse(hostnames)(getUptime)
  Await.result(allUptimes, 1.second) // List[Int] = List(1020, 960, 840)

  import cats.Applicative
  import cats.instances.future._ // for Applicative
  import cats.syntax.applicative._ // for pure
  import cats.syntax.apply._ // for mapN


  // combining accumulator and hostname using an applicative
  def newCombine(accum: Future[List[Int]], host: String): Future[List[Int]] =
    (accum, getUptime(host)).mapN(_ :+ _)

  def listTraverse[F[_]: Applicative, A, B]
    (list: List[A])(func: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]) {
      (accum, item) =>
        (accum, func(item)).mapN(_ :+ _)
    }

  def listSequence[F[_]: Applicative, B](list: List[F[B]]): F[List[B]] =
    listTraverse(list)(identity)

  val totalUptime2: Future[List[Int]] = listTraverse(hostnames)(getUptime)
  Await.result(totalUptime2, 1.second)


  // traversing with vectors
  import cats.instances.vector._ // for Applicative
  println(listSequence(List(Vector(1,2), Vector(3,4))))

  // transversing with Options
  import cats.instances.option._ // for Applicative
  def process(input: List[Int]) =
    listTraverse(input)(n => if(n%2 == 0) Some(n) else None)

  process(List(2, 4, 6))
  process(List(1, 2, 3))

  // traversing with validated
  import cats.data.Validated
  import cats.instances.list._ // for monoid
  type ErrorsOr[A] = Validated[List[String], A]
  def processValidated(input: List[Int]): ErrorsOr[List[Int]] =
    listTraverse(input) { n =>
      if(n%2 == 0) Validated.valid(n)
      else Validated.invalid(List(s"$n is not even"))
    }

  println(processValidated(List(2,4,6)))
  println(processValidated(List(1,3,2)))

  // Transverse in Cats
  trait Transverse[F[_]] {
    def traverse[G[_]: Applicative, A, B]
      (inputs: F[A])(func: A => G[B]): G[F[B]]
    def sequence[G[_]: Applicative,B]
      (inputs: F[G[B]]): G[F[B]] =
      traverse(inputs)(identity)
  }

  import cats.Traverse
  import cats.instances.future._ // for Applicative
  import cats.instances.list._ // for Traverse

  val totalUptime: Future[List[Int]] =
    Traverse[List].traverse(hostnames)(getUptime)

  val numbers = List(Future(1), Future(2), Future(3))
  val numbers2 = Traverse[List].sequence(numbers)

  import cats.syntax.traverse._
  hostnames.traverse(getUptime)
  numbers.sequence
}
