package part02.chapter08

object TestAsynchronousCode extends App {
  import cats.Id

  import scala.concurrent.Future
  trait UptimeClient[F[_]] {
    def getUptime(hostname: String): F[Int]
  }
  import cats.Applicative
  import cats.instances.list._
  import cats.syntax.functor._
  import cats.syntax.traverse._
  class UptimeService[F[_]: Applicative](client: UptimeClient[F]) {
    def getTotalUptime(hostnames: List[String]): F[Int] =
      hostnames.traverse(client.getUptime).map(_.sum)
  }
  trait RealUptimeClient extends UptimeClient[Future] {
    def getUptime(hostname: String): Future[Int]
  }
/*  trait TestUptimeClient extends UptimeClient[Id] {
    def getUptime(hostname: String): Int
  }*/
  class TestUpTimeClient(hosts: Map[String, Int]) extends UptimeClient[Id] {
    override def getUptime(hostname: String): Int =
      hosts.getOrElse(hostname, 0)
  }
  def testTotalUptime(): Unit = {
    val hosts = Map("host1" -> 10, "host2" -> 6)
    val client = new TestUpTimeClient(hosts)
    val service = new UptimeService(client)
    val actual = service.getTotalUptime(hosts.keys.toList)
    val expected = hosts.values.sum
    assert(actual == expected) // we can't compare between Future[Int]
  }
}
