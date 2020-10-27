package rocktheJvm.part3datamanipulation

import cats.kernel.Semigroup

import scala.annotation.tailrec

object DataValidation {
  import cats.data.Validated
  val aValidValue: Validated[String, Int] = Validated.valid(42) // "right" value
  val anInvalidValue: Validated[String, Int] =
    Validated.invalid("Something went wrong") // "left" value
  val aTest: Validated[String, Int] =
    Validated.cond(test = true, 90, "meaning of life is too small")

  // TODO: user Either
  /*
   *  n must be prime
   *  n must be non-negative
   *  n <= 100
   *  n must be even
   *
   * */
  import cats.instances.list._ // for semigroup
  implicit val combineInt: Semigroup[Int] = Semigroup.instance[Int]((x, _) => x)
  def isNonNegative(n: Int): Validated[List[String], Int] =
    Validated.cond(n < 0, n, List("number is negative"))
  def isLessOrEqThan100(n: Int): Validated[List[String], Int] =
    Validated.cond(n <= 100, n, List("numbers is greater that 100"))
  def testPrime(n: Int): Boolean = {
    @tailrec
    def tailRecPrime(d: Int): Boolean =
      if (d <= 1) true
      else n % d != 0 && tailRecPrime(d - 1)
    if (n == 0 || n == 1 || n == -1) false
    else tailRecPrime(Math.abs(n / 2))
  }
  def isPrime(n: Int): Validated[List[String], Int] =
    Validated.cond(testPrime(n), n, List("Number is not prime"))

  def isNotEven(n: Int): Validated[List[String], Int] =
    Validated.cond(n % 2 == 0, n, List("Number must be even"))

  def testNumber(n: Int): Validated[List[String], Int] =
    isNonNegative(n)
      .combine(isLessOrEqThan100(n))
      .combine(isPrime(n))
      .combine(isNotEven(n))

  // chain
  aValidValue.andThen(_ => anInvalidValue)
  // test a valid value
  aValidValue.ensure(List("Something went wrong"))(_ % 2 == 0)

  // transform
  aValidValue.map(_ + 1)
  aValidValue.leftMap(_.length)
  aValidValue.bimap(_.length, _ + 1)

  // TODO 2 - form validation
  /*
   *  fields are :
   *   - name
   *   - email
   *   - password
   * */
  object FormValidation {
    import cats.instances.string._
    type FormValidation[T] = Validated[List[String], T]
    def validForm(form: Map[String, String]): FormValidation[String] = {
      val validateEmail = Validated
        .fromOption(form.get("email"), List("key email does not exist"))
        .ensure(List("email does not have @"))(_.contains("@"))
      val validatedName = Validated
        .fromOption(form.get("name"), List("key name does not exist"))
        .ensure(List("name can't be blank"))(_.trim.nonEmpty)

      val validatePassword = Validated
        .fromOption(form.get("password"), List("key password does not exist"))
        .ensure(List("password is not strong"))(_.length >= 10)
      validateEmail
        .combine(validatedName)
        .combine(validatePassword)
        .map(_ => "SUCCESS")
    }
  }

  import cats.syntax.validated._
  val aValidVal: Validated[List[String], Int] = 42.valid[List[String]]
  val anInvalidVal: Validated[List[String], Int] =
    List("Something went wrong").invalid[Int]

  def main(args: Array[String]): Unit = {
    println(testNumber(5))
    println(
      FormValidation.validForm(
        Map(
          "name" -> "ernestochero",
          "email" -> "123@gmail.com",
          "password" -> "12345ABCAD"
        )
      )
    )
  }

}
