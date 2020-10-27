package rocktheJvm.part3datamanipulation

import cats.Id

object Readers {
  case class Configuration(
    dbUsername: String,
    dbPassword: String,
    host: String,
    port: Int,
    nThreads: Int,
    emailReplyTo: String
                          )
  case class DbConnection(username: String, password:String) {
    def getOrderStatus(orderId: Long): String = "dispatched"
    def getLastOrderId(username: String): Long = 542643 // selected by SQL
  }
  case class HttpServer(host: String, port: Int) {
    def start():Unit = println("Server started")
  }

  //bootstrap
  val config: Configuration = Configuration("ernesto", "password!", "localhost", 1234, 8, "123@gmail.com")
  import cats.data.Reader
  val dbReader: Reader[Configuration, DbConnection] = Reader(conf =>
    DbConnection(conf.dbUsername, conf.dbPassword)
  )
  val dbConn: Id[DbConnection] = dbReader.run(config)
  // Reader[I,O]
  val ernestoOrderStatusReader: Reader[Configuration, String] = dbReader.map(_.getOrderStatus(55))
  val ernestoOrderStatus: Id[String] = ernestoOrderStatusReader.run(config)


  def getLastOrderStatus(username: String): Id[String] = {
    (for {
        orderId <- dbReader.map(_.getLastOrderId(username))
        status <- dbReader.map(_.getOrderStatus(orderId))
      } yield status).run(config)
  }

  val httpServer: Reader[Configuration, HttpServer] = Reader( conf =>
    HttpServer(conf.host, conf.port)
  )

  /*
  * Pattern
  * 1. you create the initial data Structure
  * 2. you create a reader which specifies how the data structure will be manipulated later
  * 3. you can then map & flatmap to produce derived information
  * 4. when you need the final piece of information, you call run on the reader with the initial data structure
  * */

  case  class EmailService(emailReplyTo: String) {
    def sendEmail(address: String, contents: String) =
      s"From: $emailReplyTo; to: $address >>> $contents"
  }

  // TODO
  def emailUser(username: String, userEmail: String): Id[String] = {
    val emailServiceReader: Reader[Configuration, EmailService] = Reader (conf =>
      EmailService(conf.emailReplyTo)
    )
    (for {
      orderId <- dbReader.map(_.getLastOrderId(username))
      status <- dbReader.map(_.getOrderStatus(orderId))
      emailService <- emailServiceReader
    } yield emailService.sendEmail(
        userEmail,s"Your last order has the status: $status")
      ).run(config)
  }

  // dependency injection

  def main(args: Array[String]): Unit = {
    println(getLastOrderStatus("ernesto"))
    println(emailUser("ernesto", "abcd@gmail.com"))
  }
}
