package bonus

object PartiallyAppliedFunctions {
  def incrementMethod(x:Int): Int = x + 1
  PartiallyAppliedFunctions.incrementMethod(3) // 4
  this.incrementMethod(3) // 4

  val incrementFunction: Int => Int = (x: Int) => x + 1
  val three: Int = incrementFunction(2)

  val incrementFunctionExplicit: Int => Int = new Function[Int, Int] { // this is identical to lambda function
    override def apply(v1: Int): Int = v1 + 1
  }

  // eta-expansion
  val incrementF: Int => Int = incrementMethod

  def multiArgAdder(x: Int)(y: Int): Int = x + y
  val add2: Int => Int = multiArgAdder(2)

  def add(x:Int, y: Int, z: Int): Int = x + y + z
}
