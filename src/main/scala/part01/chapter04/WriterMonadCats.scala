package part01.chapter04

import cats.Id
import cats.data.WriterT

object WriterMonadCats extends App {
  import cats.data.Writer
  import cats.instances.vector._
  import cats.syntax.applicative._
  import cats.syntax.writer._
  val writer: WriterT[Id, Vector[String], Int] = Writer(
    Vector(
      "It was the best of times",
      "it was the worst of times"
    ),
    1859
  )

  type Logged[A] = Writer[Vector[String], A]
  // just result
  123.pure[Logged]

  // just log
  Vector(
    "msg1", "msg2", "msg3"
  ).tell

  // both - log and result
  val a = Writer(Vector( "msg1", "msg2", "msg3"), 123)
  val b = 123.writer(Vector( "msg1", "msg2", "msg3"))
  println(a.value)
  println(a.written)
  println(a.run)

  // using with map and flapMap
  val writer1 = for {
    a <- 10.pure[Logged]
    _ <- Vector("a","b", "c").tell
    b <- 32.writer(Vector("x", "y", "z"))
  } yield a + b
  val writer2 = writer1.mapWritten(_.map(_.toUpperCase))
  println(writer1.run)
  println(writer2.run)

  // exercise
  def slowly[A](body: => A) =
    try body finally Thread.sleep(100)
  def factorial(n: Int): Int = {
    val ans = slowly {
      if (n == 0) 1 else n * factorial(n - 1)
    }
    println(s"fact $n $ans")
    ans
  }
  factorial(5)
  import scala.concurrent._
  import scala.concurrent.ExecutionContext.Implicits._
  import scala.concurrent.duration._

  def execute[T](factorials: Vector[Future[T]]) =
    Await.result(Future.sequence(factorials), 5.seconds)
  println(" _Parallel execution_ ")
  execute(Vector(Future(factorial(5)), Future(factorial(5))))

  // solution with Writer Monad
  type LoggedFactorial[A] = Writer[Vector[String], A]
/*  implicit val LoggedFactorialInt: LoggedFactorial[Int] = ???
  def executeWriter[A: LoggedFactorial](factorials: Vector[Future[LoggedFactorial[A]]]) = {
    Await.result(
      Future.sequence(factorials).map(_.map(_.written)),
      5.seconds
    )
  }*/

  def writerFactorial(n: Int): LoggedFactorial[Int] = {
   for {
    ans <- if (n == 0) 1.pure[LoggedFactorial] else slowly(writerFactorial(n - 1).map(_ * n))
    _ <- Vector(s"fact $n $ans").tell
   } yield ans
 }
  println(" _Parallel Writer execution_ ")
  println {
    Await.result(
      Future.sequence {
        Vector(Future(writerFactorial(5)), Future(writerFactorial(5)))
          .map(_.map(_.written))
      }, 5.seconds
    )
  }
}
