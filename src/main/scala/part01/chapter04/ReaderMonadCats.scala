package part01.chapter04

object ReaderMonadCats extends App {
  import cats.data.Reader
  import cats.syntax.applicative._
  final case class Cat(name: String, favoriteFood: String)
  val catName: Reader[Cat, String] = Reader(cat => cat.name)
  catName.run(Cat("Garfield", "lasagne"))
  val greetKitty: Reader[Cat, String] = catName.map(name => s"Hello $name")
  greetKitty.run(Cat("Heathcliff", "junk food"))
  val feedKitty: Reader[Cat, String] =
    Reader(cat => s"Have a nice bowl of ${cat.favoriteFood}")
  val greetAndFeed: Reader[Cat, String] =
    for {
      greet <- greetKitty
      feed <- feedKitty
    } yield s"$greet. $feed"

  final case class Db(
    usernames: Map[Int, String],
    passwords: Map[String, String]
                     )
  type DbReader[A] = Reader[Db, A]
  def findUsername(userId: Int): DbReader[Option[String]] =
    Reader(db => db.usernames.get(userId))

  def checkPassword(
    username: String,
    password: String
                   ):DbReader[Boolean] =
    Reader(_.passwords.get(username).fold(false)(_ == password))

  def checkLogin(userId: Int, password: String): DbReader[Boolean] =
    for {
      username <- findUsername(userId)
      isValid <- username.map(checkPassword(_, password)).getOrElse {
        false.pure[DbReader]
      }
    } yield isValid

  val users = Map(
    1 -> "dade",
    2 -> "kate",
    3 -> "margo"
  )

  val passwords = Map(
    "dade" -> "zerocool",
    "kate" -> "acidburn",
    "margo" -> "secret"
  )

  val db = Db(users, passwords)
  println(checkLogin(1, "zerocool").run(db))
}
