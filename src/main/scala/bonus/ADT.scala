package bonus

object ADT {

  // product Type [case class, Tuple, etc]
  case class User(id: Long, username: String) // Think AND

  // the user is an Id with username ...

  // sum Type [Either, sealed trait, CoProduct]
  sealed trait Animal // Think OR
  case class Cat(name: String) extends Animal
  case class Dog(name: String) extends Animal

  // sum Type [Either, sealed trait, CoProduct]
  sealed trait VehicleType
  case object Car extends VehicleType
  case object Moto extends VehicleType
  // 2 VehicleTypes

  sealed trait Colour
  case object Red extends Colour
  case object Yellow extends Colour
  case object Blue extends Colour
  // 3 Colours

  case class Vehicle(vehicleType: VehicleType, colour: Colour, isUsed: Boolean)
  // how many vehicles ? if 2 VehicleTypes and 3 Colours and 2 Boolean types: 12 vehicles

  // Function Type
  // functions is a type in Scala -> represent exponential operation
  def like(colour: Colour): Boolean = ???
  val likeVal: Colour => Boolean = ???
  type Like = Colour => Boolean

  /**
    * Red -> {t, f}
    * Yellow -> {t, f}
    * Blue -> {t, f}
    *  there can be 2 * 2 * 2 = 8 implementations
    */

  sealed trait Country
  case object France extends Country
  case object US extends Country
  case object PERU extends Country

  sealed trait Brand
  case object Brand1 extends Brand
  case object Brand2 extends Brand
  case object Brand3 extends Brand
  case object Brand4 extends Brand

  def isProduced(brand: Brand): Country = ???
  // How many implementations ?
  // Brand1 -> {France, US, PERU}
  // ...
  // 3 * 3 * 3 * 3 = (3)^4  implementations
  type IsProduced = Brand => Country
  // corresponds to the exponential (Country)^Brand

  // Practical example
  // (A => B) => ((B => C) => C)
}

object Presentation {
  // Pure function not side effect
  def pureSum(a: Int, b: Int): Int = a + b

  // Not Pure function -> side effect: update value of variable
  var number = 10
  def notPureSum(a: Int, b: Int): Int = {
    number += 2
    a + b
  }

  def main(args: Array[String]): Unit = {}
}
