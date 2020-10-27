object ScalaIndra {
  //
  var a: Int = 10
  a+=20

  //
  val nInt: Int = 10
  val nBool: Boolean = true

  // conditionals
  val n2Bool: Boolean = 10 > 20
  val result: String = if(n2Bool) "ABC" else "D"

  //
  def sumDef(a: Int, b: Int): Int = a + b
  val sumVal: (Int, Int) => Int = (a,b) => a + b

  //
  type Transform[A] =  A => A
  // def MyFunct: Transform[Int] = { a => a + 10 }

  //
  trait Animal {
    def eat(): String
  }

  object Cat extends Animal {
    override def eat(): String = "the cat is eating"
  }

  object Dog extends Animal {
    override def eat(): String = "the dog is eating"
  }

  //
  trait Operation[T] {
    def combine(a: T, b: T): T
  }

  implicit object combineInt extends Operation[Int] {
    override def combine(a: Int, b: Int): Int = a + b
  }

  implicit object combineString extends Operation[String] {
    override def combine(a: String, b: String): String = s"$a,$b"
  }

  case class ShoppingCart(products: List[String], total: Double)
  implicit object combineShoppingCart extends Operation[ShoppingCart] {
    override def combine(a: ShoppingCart, b: ShoppingCart): ShoppingCart =
      ShoppingCart(
        a.products ++ b.products,
        a.total + b.total
      )
  }


  // API
  def combineTwoTypes[T](a:T, b:T)(implicit operation: Operation[T]):T =
    operation.combine(a,b)

  // wrappers
  // Option -> Some[T](value:T) -> None
  val mapUsers: Map[Int, String] =
    Map(
      1 -> "Gaston",
      2 -> "Juan"
    )
  val aOptionInt: Option[Int] = Some(30)
  val aUser: Option[String] = mapUsers.get(1)   // Some(Gaston)
  val aNanUser: Option[String] = mapUsers.get(100)      // None
  val aDefaultUser: String = mapUsers.getOrElse(100, "default")
  def evaluateUser(user: Option[String]): String = user match {
    case Some(username) => username
    case None => "default"
  }

  // Try[T] -> Success[T](value:T) -> Failure[E <: Throwable](err: Throwable)
  // Either[A, B] -> Left(a:A) -> Right(b:B)
  // Future
  trait Func[F[_]] {
    def map[A,B](fa: F[A])(f: A => B): F[B]
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  }

  val upper: String => String = str => str.toUpperCase
  def upper2(str:String):String = str.toUpperCase
  val length: String => Int = str => str.length
  val aUpperCaseUser: Option[String] = aUser.map(upper) // GASTON
  val aUpperCaseUser2: Option[Int] = aUser.map(str => upper2(str)).map(length) // GASTON
  val aUserLength: Option[Int] = aUser.map(_.toUpperCase).map(_.length) // GASTON


  val aUpperCaseUserResult: Option[(String, Int)] = aUpperCaseUser.flatMap(user1 => aUserLength.map(lengthU => (user1, lengthU)))
  val aUpperCaseUserResult1: Option[(String, Int)] = for {
    user1 <- aUpperCaseUser
    lengthU <- aUserLength
  } yield (user1, lengthU)



  // Unit <==> void
  //

  def main(args: Array[String]): Unit = {
    println(Cat.eat())
    println(Dog.eat())
    println(combineTwoTypes(10,20))
    println(combineTwoTypes("a","b"))
    println(
      combineTwoTypes(
        ShoppingCart(List("X"), 10),
        combineTwoTypes(
          ShoppingCart(List("A","B"), 40),
          ShoppingCart(List("C"), 60)
        )
      )
    )
    // println(combineTwoTypes(10.0,10.0)) // not compile
    println(evaluateUser(aUser))
    println(evaluateUser(aNanUser))
    println(aUpperCaseUser)
    //
    println(aUpperCaseUserResult)
    println(aUpperCaseUserResult1)
  }
}
