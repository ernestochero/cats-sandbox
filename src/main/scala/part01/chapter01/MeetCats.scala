package part01.chapter01

import cats.Show
import cats.implicits._
object MeetCats extends App {
  case class Person(name: String, age: Int)
  val f = (p: Person) => s"My name is ${p.name} and I'm ${p.age} years old and I'm adult."
  val g = (p: Person) => s"My name is ${p.name} and I'm ${p.age} years old and I'm not adult."
  def condition(age: Int): Person => String = if (age >= 18) f else g
  implicit val personShow: Show[Person] = Show.show[Person] { person => condition(person.age)(person) }
  val ernesto = Person("Ernesto", 15)
  println(ernesto.show)

  // Example custom Eq
  trait MyEq[A] {
    def eqv(a:A, b:A): Boolean
  }
  object MyEq{
    def apply[A](implicit myEq: MyEq[A]): MyEq[A] = myEq
    def compare[A](a: A, b: A)(implicit myEq: MyEq[A]): Boolean = {
      myEq.eqv(a,b)
    }
  }
  object MyEqInstances {
    implicit val intEqv: MyEq[Int] = (a: Int, b: Int) => a == b
  }
  import MyEqInstances._
  println(MyEq[Int].eqv(10,12))
  println(MyEq.compare(10, 12))


  // Example Eq with cats
  import cats.Eq
  import cats.instances.int._
  import cats.syntax.option._
  val eqInt = Eq[Int]
  println(eqInt.eqv(10,10))
  import cats.syntax.eq._
  println(123 === 123)

  implicit val eqPerson: Eq[Person] = Eq.instance[Person] { (x: Person, y: Person) =>
    x.name === y.name && x.age === y.age
  }
  println(Person("ernesto",24) === Person("ernesto", 24))
  1.some === none[Int]
  Some(1).flatMap(_.some) === Some(10).flatMap(_.some)

  // exercise Cats
  final case class Cat(name: String, age: Int, color: String)
  implicit val eqCat: Eq[Cat] = Eq.instance[Cat] { (x: Cat , y: Cat) =>
    x.name === y.name && x.age === y.age && x.color === y.color
  }
  val cat1 = Cat("Garfield", 38, "orange and black")
  val cat2 = Cat("Heathcliff", 33, "orange and black")
  val optionCat1 = Option(cat1)
  val optionCat2 = Option.empty[Cat]
  println("Eq Cats result : ")
  println(cat1 === cat2)
  println(optionCat1 =!= optionCat2)
}
