package rocktheJvm.part2abstractMath

object Monoids {
  import cats.instances.int._
  import cats.syntax.semigroup._ // import the |+| extension method
  val numbers: List[Int] = (1 to 1000).toList
  // |+| is always associative
  val sumLeft: Int = numbers.foldLeft(0)(_ |+| _)
  val sumRight: Int = numbers.foldRight(0)(_ |+| _)

  // define a general API
  import cats.Monoid
  val intMonoid: Monoid[Int] = Monoid[Int]
  val zero: Int = intMonoid.empty // 0

  import cats.instances.string._
  val stringMonoid: Monoid[String] = Monoid[String]
  val emptyString: String = stringMonoid.empty // ""

  import cats.instances.option._
  val zeroOption: Option[Int] = Monoid[Option[Int]].empty // None
  val combineOption: Option[Int] = Monoid[Option[Int]].combine(Option(2), Option.empty[Int]) // Some(2)

  // extension methods for Monoids - |+|
  val combineOptionFancy: Option[Int] = Option(3) |+| Option(2) // Some(5)

  // TODO 1: implement a combineFold
  def combineFold[T: Monoid](list: List[T]):T =
    list.foldLeft(Monoid[T].empty)(_ |+| _)

  // TODO 2: combine a list of phone books as Maps[String, Int]
  // hint: don't construct a monoid - use an import
  val phoneBooks = List(
    Map(
      "Alice" -> 325,
      "Bob" -> 647
    ),
    Map(
      "Charlie" -> 372,
      "Daniel" -> 889
    ),
    Map(
      "Tina" -> 123,
      "Daniel" -> 1
    )
  )
  import cats.instances.map._
  val phoneBooksResult: Map[String, Int] = combineFold(phoneBooks)
  // TODO 3:
  case class ShoppingCart(items: List[String], total: Double)
  def checkout(shoppingCarts: List[ShoppingCart]): ShoppingCart =
    combineFold(shoppingCarts)
  implicit val shoppingCartMonoid: Monoid[ShoppingCart] = Monoid.instance[ShoppingCart](
    ShoppingCart(List.empty[String], 0), (sh1, sh2) => {
      ShoppingCart(
        sh1.items ++ sh2.items,
        sh1.total + sh2.total
      )
    })

  val shoppingCarts = List(ShoppingCart(List("A", "B"), 22), ShoppingCart(List("C", "D"), 22))


  def main(args: Array[String]): Unit = {
    println(s"sumLeft $sumLeft is the same that sumRight $sumRight")
    println(combineFold(numbers))
    println(phoneBooksResult)
    println(checkout(shoppingCarts))
  }
}
