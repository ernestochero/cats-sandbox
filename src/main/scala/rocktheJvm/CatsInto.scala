package rocktheJvm

object CatsInto {
  // 1. type class import
  import cats.Eq

  // 2. import type class instances for the type you need
  import cats.instances.int._

  // 3. use the type class API
  val intEquality: Eq[Int] = Eq[Int]
  val aTypeSafeComparison: Boolean = intEquality.eqv(10, 20)
  // val anUnsafeComparison = intEquality.eqv(2, "a string")

  // 4. user extension methods (if applicable)
  import cats.syntax.eq._
  val anotherTypeSafeComp: Boolean = 2 === 3

  // 5. extending the Type class operation to composite types. e.g list
  import cats.instances.list._
  val aListComparison: Boolean = List(2) === List(3)

  // 6. create a TC instance for a custom type
  case class ToyCar(model: String, price: Double)
  implicit val toyCarEq: Eq[ToyCar] = Eq.instance[ToyCar]( (car1, car2) => car1.price == car2.price )
  val compareTwoToyCars: Boolean = ToyCar("Ferrari", 29.99) === ToyCar("Lamborghini", 29.99)

  //
}
