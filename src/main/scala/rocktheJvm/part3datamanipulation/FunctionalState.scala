package rocktheJvm.part3datamanipulation

import cats.Eval
import cats.data.IndexedStateT

object FunctionalState {
  type MyState[S, A] = S => (S, A)
  import cats.data.State
  val countAndSay: State[Int, String] =
    State(currentCount => (currentCount + 1, s"Counted $currentCount"))
  val (eleven, counted10) = countAndSay.run(10).value

  // state = "iterative" computations
  var a = 10
  a += 1
  val firstComputation = s"Added 1 to 10, obtained $a"
  a *= 5
  val secondComputation = s"Multiplied with 5, obtained $a"

  // pure FP with states
  val firstTransformation: State[Int, String] =
    State((s: Int) => (s + 1, s"Added 1 to 10 => obtained $s"))
  val secondTransformation: State[Int, String] =
    State((s: Int) => (s * 5, s"Multiplied with 5 => obtained $s"))
  val composeTransformation: IndexedStateT[Eval, Int, Int, (String, String)] =
    firstTransformation.flatMap(firstResult =>
      secondTransformation.map(secondResult => (firstResult, secondResult))
    )
  composeTransformation.run(10)

  // TODO: an online store
  case class ShoppingCart(items: List[String], total: Double)
  def addToCart(item: String, price: Double): State[ShoppingCart, Double] =
    State(cart => {
      val total = cart.total + price
      val items = item :: cart.items
      (ShoppingCart(items, total), total)
    })

  val danielsCart: IndexedStateT[Eval, ShoppingCart, ShoppingCart, Double] =
    for {
      _ <- addToCart("Fender guitar", 500)
      _ <- addToCart("Elixir strings", 19)
      total <- addToCart("Electric cable", 8)
    } yield total

  // TODO 2: pure mental gymnastics
  // returns a State data structure that, when run, will not change the state but will issue the value f(a)
  def inspect[S, A](f: S => A): State[S, A] =
    State(s => (s, f(s)))

  // returns a State data structure that, when run, returns the value of that state and makes no changes
  def get[A]: State[A, A] = inspect(identity)

  // returns a State data structure that, when run, returns Unit and sets the state to that value
  def set[S](value: S): State[S, Unit] =
    State(_ => (value, ()))

  // returns a State data structure that, when run, will return Unit and sets the state to f(state)
  def modify[S](f: S => S): State[S, Unit] =
    State(s => (f(s), ()))

  val program: State[Int, (Int, Int, Int)] = for {
    a <- State.get[Int]
    _ <- State.set(a + 10)
    b <- State.get[Int]
    _ <- State.modify[Int](_ + 43)
    c <- State.inspect[Int, Int](_ * 2)
  } yield (a, b, c)

  def main(args: Array[String]): Unit = {
    println(composeTransformation.run(10).value)
    println(
      addToCart("car", 10).run(ShoppingCart(List("book", "ball"), 50)).value
    )
    println(danielsCart.run(ShoppingCart(List(), 0)).value)
    println(program.run(10).value)
  }
}
