package rocktheJvm.part2abstractMath

object SemiGroups {
  // Semigroups COMBINE elements of the same type
  import cats.Semigroup
  import cats.instances.int._
  import cats.instances.string._
  val naturalIntSemigroup: Semigroup[Int] = Semigroup[Int]
  val intCombination: Int = naturalIntSemigroup.combine(2, 46) // addition
  def reduceInts(list: List[Int]): Int = list.reduce(Semigroup[Int].combine)
  def reduceStrings(list: List[String]): String = list.reduce(Semigroup[String].combine)

  // general API
  def reduceThings[T](list: List[T])(implicit semigroup: Semigroup[T]): T =
    list.reduce(semigroup.combine)

  // TODO 1: support a new type
  case class Expense(id: Long, amount: Double)
  implicit val expenseSemigroup: Semigroup[Expense] = Semigroup.instance[Expense]((ex1, ex2) =>
    Expense(
      id = Math.max(ex1.id, ex2.id),
      amount = ex1.amount + ex2.amount
    )
  )

  // extension methods from semigroup
  import cats.instances.list._
  import cats.syntax.semigroup._
  val anIntSum: Int = 2 |+| 3
  val anListC: List[Int] = List(2, 3) |+| List(4,4) // requires the presence of an implicit Semigroup[Int]

  // TODO 2: implement reduceThings 2
  def reduceThings2[T: Semigroup](list: List[T]): T =
    list.reduce(_ |+| _)


  def main(args: Array[String]): Unit = {
    val numbers = (1 to 10).toList
    val strings = List("Hello ", "World")
    val numbersOption = numbers.map(Option(_))
    println(intCombination)
    // general API
    println(reduceThings(numbers))
    println(reduceThings(strings))
    import cats.instances.option._
    println(reduceThings(numbersOption))

    //
    val expenses = List(Expense(1,10), Expense(2,20))
    println(reduceThings(expenses))

    // Using syntax |+|
    println(anListC)

    // test ex 2
    println(reduceThings2(expenses))
  }
}
