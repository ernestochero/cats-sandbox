package rocktheJvm

object TypeClassesAndVariance {

  import cats.instances.int._
  import cats.instances.option._
  import cats.syntax.eq._
  val aComparison = Option(2) === Option(3)
 // val anInvalidComparison = Some(2) === None

  // variance
  class Animal
  class Cat extends Animal
  class Garfield extends Cat
  // covariant type: subtyping is propagated to the generic type
  class Cage[+T]
  val cage: Cage[Animal] = new Cage[Cat] // Cat <: Animal, so Cage[Cat] <: Cage[Animal]


  // contravariant type : subtyping is propagated backwards to the generic type
  class CCage[-T]
  val ccage: CCage[Garfield] = new CCage[Cat] //  Garfield <: Cat , so CCage[Cat] <: CCage[Garfield]


  // rule thumb: "HAS a T" = covariant , "ACTS on T" = contravariant
  // variance affect how Type Classes are being fetched

  // Contravariant TC
  trait SoundMaker[-T]
  implicit object AnimalSoundMaker extends SoundMaker[Animal]
  def makeSound[T](implicit soundMaker: SoundMaker[T]): Unit = println("Wow")
  makeSound[Animal] // ok - TC instance defined above
  makeSound[Cat] // ok - TC instance for animal is also applicable to cats
  // rule 1: contravariant TCs can use superclass instances if nothing is available strictly for that type
/*  implicit object OptionSoundMaker extends SoundMaker[Option[Int]]
  makeSound[Option[Int]]
  makeSound[Some[Int]]*/


  // Covariant TC
  trait AnimalShow[+T] {
    def show: String
  }
  implicit object GeneralAnimalShow extends AnimalShow[Animal] {
    override def show: String = "animals everywhere"
  }
  implicit object CatsShow extends AnimalShow[Cat] {
    override def show: String = "so many cats!"
  }

  def organizeShow[T](implicit event: AnimalShow[T]): String = event.show

  // rule 2: covariant TCs will always use the more specific TC instance for that type
  // but may confuse the compiler if the general TC is also present

  def main(args: Array[String]): Unit = {
    println(organizeShow[Cat])
    // println(organizeShow[Animal])
  }
}
