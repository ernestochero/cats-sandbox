package rocktheJvm.part2abstractMath

import java.util.concurrent.Executors

import scala.concurrent.{ExecutionContext, Future}

object MonadTransformers {
  import cats.data.OptionT
  import cats.instances.list._ // import Monad[List]

  type ListOption = OptionT[List, Int]
  def sumAllOptions(values: List[Option[Int]]): Int =
    OptionT(values).foldLeft(0)(_ + _)


  import cats.data.EitherT
  import cats.syntax.either._

  val listOfEither: EitherT[List, String, Int] = EitherT(List("something wrong".asLeft, 43.asRight, 2.asRight))

  // TODO exercise
  val bandWidths = Map (
    "server1.acme.com" -> 50,
    "server2.acme.com" -> 300,
    "server3.acme.com" -> 170
  )
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  type AsyncResponse[T] = EitherT[Future, String, T]

  def getBandWidth(server: String): AsyncResponse[Int] = bandWidths.get(server) match {
    case None => EitherT(Future(s"Server $server unreachable".asLeft))
    case Some(b) => EitherT(Future(b.asRight))
  }

  // TODO 1:
  import cats.instances.future._
  def canWithStandSurge(s1: String, s2: String): AsyncResponse[Boolean] =
    for {
      widthS1 <- getBandWidth(s1)
      widthS2 <- getBandWidth(s2)
    } yield { (widthS1 + widthS2) > 250 }

  // TODO 2:

  def generateTrafficSpikeReport(s1: String, s2: String): AsyncResponse[String] =
    canWithStandSurge(s1, s2).transform {
      case Right(true) =>
        s"Servers $s1 and $s2 CAN cope with the incoming spike NO PROBLEM!".asRight
      case Right(false) =>
        s"Servers $s1 and $s2 CANNOT cope with the incoming spike".asLeft
      case Left(msg) => s"Servers $s1 and $s2 CANNOT cope with the incoming spike: $msg".asLeft
    }

  def main(args: Array[String]): Unit = {
    val values = List(Option(10), Option(11))
    println(sumAllOptions(values))
    // show values
    import scala.concurrent.Await
    import scala.concurrent.duration._
    generateTrafficSpikeReport("server1.acme.com", "server3.acme.com").value.foreach(println)
  }
}
