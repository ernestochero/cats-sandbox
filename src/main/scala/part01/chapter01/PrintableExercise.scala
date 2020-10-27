package part01.chapter01

object PrintableExercise extends App {
  trait Printable[A] {
    def format(value: A): String
  }
  object Printable {
    def format[A](input: A)(implicit p: Printable[A]) : String =
      p.format(input)
    def print[A](input: A)(implicit p: Printable[A]): Unit = println(format(input))
  }
  object PrintableInstances {
    implicit val stringPrintable = new Printable[String] {
      override def format(value: String): String = value
    }
    implicit val intPrintable = new Printable[Int] {
      override def format(value: Int): String = value.toString
    }
  }

  import PrintableInstances._
  final case class Cat(name: String, age: Int, color: String)
  implicit val catPrintable: Printable[Cat] = (cat: Cat) => {
    val name = Printable.format(cat.name)
    val age = Printable.format(cat.age)
    val color = Printable.format(cat.color)
    s"$name is a $age year-old $color cat"
  }

  val cat = Cat("Garfield", 41, "ginger and black")
  Printable.print(cat)
}
