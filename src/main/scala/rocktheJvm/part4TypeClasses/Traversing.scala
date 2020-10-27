package rocktheJvm.part4TypeClasses

import java.util.concurrent.Executors

import cats.data.Validated
import cats.Applicative

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

object Traversing {
  implicit val ec: ExecutionContext =
    ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

  val servers: List[String] = List(
    "server-ci",
    "server-staging",
    "server-prod"
  )

  def getWidth(hostname: String): Future[Int] = Future(hostname.length * 80)
  val allBandWidthsTraverse: Future[List[Int]] =
    Future.traverse(servers)(getWidth)
  val allBandWidthSequence: Future[List[Int]] =
    Future.sequence(servers.map(getWidth))

  // TODO 1
  import cats.syntax.applicative._
  import cats.syntax.apply._

  def listTraverse[F[_]: Applicative, A, B](
      list: List[A]
  )(func: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]) { (wAccumulator, element) =>
      val wElement: F[B] = func(element)
      (wAccumulator, wElement).mapN(_ :+ _)
    }

  // TODO 2
  def listSequence[F[_]: Applicative, A](list: List[F[A]]): F[List[A]] =
    listTraverse(list)(identity)

  // TODO 3
  import cats.instances.vector._
  /*val allPairs: Vector[List[Int]] =
    listSequence(List(Vector(1, 2), Vector(3, 4))) // all the possible 2-tuples*/
  val allTriples: Vector[List[Int]] =
    listSequence(
      List(Vector(1, 2), Vector(3, 4), Vector(5, 6))
    ) // all the possible 3-tuples

  import cats.instances.option._
  def filterAsOption(list: List[Int])(
      predicate: Int => Boolean
  ): Option[List[Int]] =
    listTraverse(list)(Some(_).filter(predicate))

  val allTrue: Option[List[Int]] = filterAsOption(List(2, 4, 6))(_ % 2 == 0)
  val someFalse: Option[List[Int]] = filterAsOption(List(1, 2, 3))(_ % 2 == 0)

  type ErrorsOr[T] = Validated[List[String], T]
  import cats.instances.list._
  def filterAsValidated(list: List[Int])(
      predicate: Int => Boolean
  ): ErrorsOr[List[Int]] =
    listTraverse[ErrorsOr, Int, Int](list)(n =>
      Validated.valid(n).ensure(List(s"predicated for $n failed"))(predicate)
    )

  val allTrueValidated: ErrorsOr[List[Int]] =
    filterAsValidated(List(2, 4, 6))(_ % 2 == 0)
  val someFalseValidated: ErrorsOr[List[Int]] =
    filterAsValidated(List(1, 2, 3))(_ % 2 == 0)

  import cats.Traverse
  import cats.instances.future._
  val allBandwidthCats: Future[List[Int]] =
    Traverse[List].traverse(servers)(getWidth)

  import cats.syntax.traverse._ // sequence + traverse
  val allBandwidthCats2: Future[List[Int]] = servers.traverse(getWidth)

  import cats.instances.try_._
  val valueTry: Try[Int] = Try(10)
  val r1: Option[Try[Int]] =
    Traverse[Try].traverse[Option, Int, Int](valueTry)(Some(_))
  val r2: Try[Option[Int]] = r1.sequence

  def main(args: Array[String]): Unit = {
    //println(allPairs)
    //println(allTriples)
    println(allTrueValidated)
    println(someFalseValidated)
  }
}
