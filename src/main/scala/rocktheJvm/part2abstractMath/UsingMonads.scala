package rocktheJvm.part2abstractMath

object UsingMonads {
  import cats.Monad
  import cats.instances.list._
  val monadList = Monad[List]
  val aSimpleList = monadList.pure(2) // List(2)
  val anExtendedList = monadList.flatMap(aSimpleList)(x => List(x, x + 1)) // List(2,3)
  //applicable to Option, Try, Future

  val aManualEither: Either[String, Int] = Right(42)

  type LoadingOr[T] = Either[String, T]
  type ErrorOr[T] = Either[Throwable, T]

  import cats.instances.either._
  val loadingMonad: Monad[LoadingOr] = Monad[LoadingOr]
  val errorMonad: Monad[ErrorOr] = Monad[ErrorOr]
  val anEither: LoadingOr[Int] = loadingMonad.pure(42) // LoadingOr[Int] == Right(45)
  val aChangedLoading: LoadingOr[Int] =
    loadingMonad.flatMap(anEither)(n => if (n%2==0) Right(n + 1) else Left("Loading meaning of life ...") )


  // imaginary online store
  case class OrderStatus(orderId: Long, status: String)
  def gerOrderStatus(orderId: Long): LoadingOr[OrderStatus] =
    Right(OrderStatus(orderId, "Ready to ship"))

  def trackLocation(orderStatus: OrderStatus): LoadingOr[String] =
    if(orderStatus.orderId > 1000) Left("Not available yet, refreshing data ... ")
    else Right("Amsterdam, NL")

  import cats.syntax.flatMap._
  import cats.syntax.functor._
  val orderLocation: LoadingOr[String] = gerOrderStatus(457L).flatMap(trackLocation)
  val orderLocation2: LoadingOr[String]  =
    for {
      st <- gerOrderStatus(457L)
      location <- trackLocation(st)
    } yield location

  // TODO : the service layer API of a web app
  case class Connection(host: String, port: String)
  val conf = Map(
    "host" -> "localhost",
    "port" -> "4040"
  )

  trait HttpService[M[_]] {
    def getConnection(cfg: Map[String, String]): M[Connection]
    def issueRequest(connection: Connection, playLoad: String): M[String]
  }

  def getResponse[M[_]: Monad](service: HttpService[M], payload: String): M[String] =
    for {
      conn <- service.getConnection(conf)
      response <- service.issueRequest(conn, payload)
    } yield response

  // DO NOT CHANGE THE CODE
  import scala.util.{Failure, Success, Try}
  // TODO : provide a real implementation of  HttpService using Try, Option, Future, Either
  def buildConnection(cfg: Map[String, String]): Option[Connection] =
    for {
      host <- cfg.get("host")
      port <- cfg.get("port")
    } yield Connection(host, port)

  implicit object TryHttpService extends HttpService[Try] {
    override def getConnection(cfg: Map[String, String]): Try[Connection] =
      buildConnection(cfg) match {
        case Some(connection) => Success(connection)
        case None => Failure(new Throwable("fail"))
      }
    override def issueRequest(connection: Connection, playLoad: String): Try[String] =
      if(playLoad.length >= 20) Failure(new Throwable("fail"))
      else Success(s"request $playLoad has been accepted")
  }

  implicit object OptionHttpService extends HttpService[Option] {
    override def getConnection(cfg: Map[String, String]): Option[Connection] = buildConnection(cfg)
    override def issueRequest(connection: Connection, playLoad: String): Option[String] = {
      if(playLoad.length >= 100) None
      else Some(s"request $playLoad has been accepted")
    }
  }

  // TODO implement another HttpService with ErrorOr
  implicit object ErrorOrHttpService extends HttpService[ErrorOr] {
    override def getConnection(cfg: Map[String, String]): ErrorOr[Connection] =
      buildConnection(cfg) match {
        case Some(connection) => Right(connection)
        case None => Left(new RuntimeException("Connection could not be established: invalid configuration"))
      }
    override def issueRequest(connection: Connection, playLoad: String): ErrorOr[String] =
      if(playLoad.length >= 20) Left(new Throwable("fail"))
      else Right(s"request $playLoad has been accepted")
  }



  def main(args: Array[String]): Unit = {
    println(getResponse(ErrorOrHttpService, "Hello, ErrorOR"))
    import cats.instances.option._
    println(getResponse(OptionHttpService, "Hello, Option"))
  }
}
