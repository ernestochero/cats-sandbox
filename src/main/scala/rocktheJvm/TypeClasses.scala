package rocktheJvm

object TypeClasses {
  case class Person(name: String, age: Int)
  // 1. type class definition
  trait JSONSerializer[T] {
    def toJson(value: T): String
  }

  // 2. create implicit type class INSTANCES
  implicit object StringSerializer extends JSONSerializer[String] {
    override def toJson(value: String): String = "\"" + value + "\""
  }
  implicit object IntSerializer extends JSONSerializer[Int] {
    override def toJson(value: Int): String = value.toString
  }
  implicit object PersonSerializer  extends JSONSerializer[Person] {
    override def toJson(value: Person): String =
      s"""
         |{ "name" : ${value.name}, "age" : ${value.age} }
         |""".stripMargin.trim
  }

  // 3. offer some API to use
  def convertListToJson[T](list: List[T])(implicit serializer: JSONSerializer[T]): String =
    list.map(v => serializer.toJson(v)).mkString("[", "," ,"]")

  // 4. extending the existing type via extension methods
  object JSONSyntax {
    implicit class JSONSerializable[T](value: T)(implicit serializer: JSONSerializer[T]) {
      def toJson: String = serializer.toJson(value)
    }
  }

  def main(args: Array[String]): Unit = {
    println(convertListToJson(List(Person("Alice", 23), Person("Ernesto", 24))))
    import JSONSyntax._
    val bob = Person("Bob", 23)
    println(bob.toJson)
  }

}
