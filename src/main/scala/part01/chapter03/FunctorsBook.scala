package part01.chapter03

object FunctorsBook extends App {
  import cats.Functor
  import cats.instances.option._
  import cats.instances.list._
  println(Functor[Option].map(Option("Hello"))(_.length))
  println(Functor[Option].map(Option.empty[String])(_.length))
  val lenOption: Option[String] => Option[Int] = Functor[Option].lift(_.length)
  println(lenOption(Some("Hello")))
  val source = List("Cats", "is", "awesome")
  val product = Functor[List].fproduct(source)(_.length).toMap
  println(product)
  val listOpt = Functor[List] compose Functor[Option]
  println(listOpt.map(List(Some(1), None, Some(3)))(_ +1))
}

object EvalBook extends App {
  import cats.Eval
  // import cats.implicits._
  val eager = Eval.now {println("Running expensive calculation [Now]..."); 1 + 2 * 3}
  val lazyVal = Eval.later {println("Running expensive calculation [Later]..."); 1 + 2 * 3}
  eager.value
  lazyVal.value

  //
  val n = 2
  var counter = 0
  val lazyVal1 = Eval.later {
    println("This is lazily evaluated with caching")
    counter = counter + 1
    (1 to n)
  }

  List.fill(n)("").foreach(_ => lazyVal1.value)
  println(lazyVal1.value)

  val list = List.fill(3)(0)
  val evalDef: Eval[List[Int]] = Eval.now(list).flatMap(e => Eval.defer(Eval.later(e)))
  println(Eval.defer(evalDef).value)
}