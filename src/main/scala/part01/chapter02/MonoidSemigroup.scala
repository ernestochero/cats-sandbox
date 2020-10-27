package part01.chapter02

object MonoidExample extends App {
  import cats.implicits._
  val l = List(1, 2, 3, 4, 5)
  val id = l.foldMap(identity)
  val idStr = l.foldMap(_.toString)
  val tuple = l.foldMap(v => (v, v.toString))
  println(tuple)
}

object MonoidSemigroup extends App {
  trait Semigroup[A] {
    def combine(x: A, y: A): A
  }

  trait Monoid[A] extends Semigroup[A] {
    def empty: A
  }

  object Monoid {
    def apply[A](implicit monoid: Monoid[A]): Monoid[A] = monoid
  }

  implicit val booleanAndMonoid = new Monoid[Boolean] {
    override def empty: Boolean = true

    override def combine(x: Boolean, y: Boolean): Boolean = x && y
  }

/*  implicit val booleanOrMonoid = new Monoid[Boolean] {
    override def empty: Boolean = false
    override def combine(x: Boolean, y: Boolean): Boolean = x || y
  }*/

  implicit def setUnionMonoid[A]: Monoid[Set[A]] = new Monoid[Set[A]] {
    override def empty: Set[A] = Set.empty[A]
    override def combine(x: Set[A], y: Set[A]): Set[A] = x union y
  }

  val intSetMonoid = Monoid[Set[Int]]
  val strSetMonoid = Monoid[Set[String]]
  println(intSetMonoid.combine(Set(1,2), Set(2,3)))
  println(strSetMonoid.combine(Set("A", "B"), Set("B", "C")))
}

object MonoidSemigroupCats extends App {
  // With Cats
  import cats.Monoid
  import cats.instances.string._
  import cats.instances.option._
  import cats.syntax.semigroup._
  import cats.Semigroup
  val streamMonoid = Monoid[String]
  val optionMonoid = Monoid[Option[String]]
  println(streamMonoid.combine("Hello", "World"))
  println(optionMonoid.combine(Option("Hello"), Option("World")))
  println("Hello " |+| "World" |+| Monoid[String].empty)
  println(Option("Hello ") |+| Option("World") |+| Monoid[Option[String]].empty)

  // Exercise solution
  def add[A: Monoid](items: List[A]): A =
    items.foldLeft(Monoid[A].empty)(_ |+| _)

  case class Order(totalCost: Double, quantity: Double)
  implicit val orderMonoid = new Monoid[Order] {
    override def empty: Order = Order(0, 0)
    override def combine(x: Order, y: Order): Order =
      Order(
        x.totalCost + y.totalCost,
        x.quantity + y.quantity
      )
  }
  add(List(Order(10,20), Order(10,30)))
  import cats.instances.set._
  import cats.instances.int._
  Monoid[Set[Int]].combine(Set(10), Set(20))
  def addAll[A: Monoid](values: List[A]):A =
    values.foldRight(Monoid[A].empty)(_ |+| _)
  println(addAll(List(Some(3), None, Some(10))))

  // More examples
  import cats.instances.function._
  println(Semigroup[Int => Int].combine(_ + 1, _ * 10).apply(6))

  import cats.instances.map._
  val aMap = Map("foo" -> Map("bar" -> 5))
  val anotherMap = Map("foo" -> Map("bar" -> 6))
  val combinedMap = Semigroup[Map[String, Map[String, Int]]].combine(aMap, anotherMap)
  println(combinedMap.get("foo"))

  val one: Option[Int] = Option(1)
  val two: Option[Int] = Option(2)
  val n: Option[Int] = None
  println(one |+| two)
  println(n |+| Option(2))
  println(n |+| n)

  println(Monoid[String].empty)
  println(Monoid[String].combineAll(List("a", "b", "c")))

  println(
    Monoid[Map[String, Int]].combineAll(List())
  )
}