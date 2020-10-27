package part01.chapter01

object TypeClass extends App {
  sealed trait Json
  final case class JsObject(get: Map[String, Json]) extends Json
  final case class JsString(get: String) extends Json
  final case class JsNumber(get: Double) extends Json
  case object JsNull extends Json

  trait JsonWriter[A] {
    def write(value: A): Json
  }
  object Json {
    def toJson[A](value: A)(implicit w: JsonWriter[A]): Json =
      w.write(value)
  }

  final case class Person(name: String, email: String)
  object JsonWritesInstances {
    implicit val stringWriter: JsonWriter[String] =
      (value: String) => JsString(value)
    implicit val personWriter: JsonWriter[Person] =
      (person: Person) => JsObject(Map(
        "name" -> JsString(person.name),
        "email" -> JsString(person.email)
      ))

    implicit def optionWriter[A](implicit writer: JsonWriter[A]): JsonWriter[Option[A]] =
      new JsonWriter[Option[A]] {
        override def write(value: Option[A]): Json = value.fold[Json](JsNull)(writer.write)
      }
  }

  object JsonSyntax {
    implicit class JsonWritesOps[A](value: A) {
      def toJson(implicit w: JsonWriter[A]): Json = w.write(value)
    }
  }

  import JsonWritesInstances._
  val json = Json.toJson(Person("Ernesto", "ernesto@gmail.com"))
  val opt: Option[String] = None
  val jsonOpt = Json.toJson(opt)
  println(json)
  println(jsonOpt)
}
