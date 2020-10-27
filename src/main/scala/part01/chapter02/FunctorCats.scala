package part01.chapter02

object FunctorCats extends App {
  import cats.instances.function._
  import cats.syntax.functor._
  import cats.Functor
  val func1: Int => Double =
    (x: Int) => x.toDouble

  val func2: Double => Double =
    (y: Double) => y * 2

  (func1 andThen func2)(1)
  (func1 map func2)(1)
  func2(func1(1))

  import cats.instances.list._
  import cats.instances.option._

  val list1 = List(1, 2, 3)
  val list2 = Functor[List].map(list1)(_ * 2)

  val option1 = Option(123)
  val option2 = Functor[Option].map(option1)(_.toString)

  val func = (x: Int) => x + 1
  val liftedFunc = Functor[Option].lift(func)
  val list = Functor[List].as(list1, "As")

  def doMath[F[_]](start: F[Int])(implicit functor: Functor[F]): F[Int] =
    start.map(n => n + 1 * 2)

  import cats.instances.option._
  import cats.instances.list._
  doMath(Option(20))
  doMath(List(1,2,3))

  final case class Box[A](value: A)

  val box = Box[Int](123)
  implicit val boxFunctor = new Functor[Box] {
    override def map[A, B](fa: Box[A])(f: A => B): Box[B] =
      Box(f(fa.value))
  }
  box.map(_ + 10)
  Functor[Box].map(box)(_ + 10)

  sealed trait Tree[+A]
  final case class Branch[A](left:Tree[A], right: Tree[A]) extends Tree[A]
  final case class Leaf[A](value: A) extends Tree[A]
  implicit val treeFunctor = new Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] =
      fa match {
        case Branch(left, right) =>
            Branch(map(left)(f), map(right)(f))
        case Leaf(value) => Leaf(f(value))
      }
  }
  object Tree {
    def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
      Branch(left, right)
    def leaf[A](value: A): Tree[A] = Leaf(value)
  }

  Tree.branch(Leaf(10), Leaf(20)).map(_ * 2)

  trait Printable[A] { self =>
    def format(value: A): String
    def contramap[B](func: B => A) =
      new Printable[B] {
        override def format(value: B): String = self.format(func(value))
      }
  }

  def globalFormat[A](value: A)(implicit p: Printable[A]): String =
    p.format(value)

  implicit val stringPrintable: Printable[String] = new Printable[String] {
    override def format(value: String): String = s"'${value}'"
  }
  implicit val booleanPrintable = new Printable[Boolean] {
    override def format(value: Boolean): String =
      if(value) "yes" else "no"
  }

/*  implicit def boxPrintable[A](implicit pa: Printable[A]): Printable[Box[A]] = new Printable[Box[A]] {
    override def format(value: Box[A]): String = pa.format(value.value)
  }*/
  implicit def boxPrintable[A](implicit pa: Printable[A]): Printable[Box[A]] =
    pa.contramap[Box[A]](_.value)

  println(globalFormat("Hello"))
  println(globalFormat(true))
  println(globalFormat(Box("Hello World")))
  println(globalFormat(Box(true)))


  trait Codec[A] { self =>
    def encode(value: A): String
    def decode(value: String): A
    def imap[B](dec: A => B, enc: B => A): Codec[B] =
      new Codec[B] {
        override def encode(value: B): String = self.encode(enc(value))
        override def decode(value: String): B = dec(self.decode(value))
      }
  }

  def encode[A](value: A)(implicit c: Codec[A]): String = c.encode(value)
  def decode[A](value: String)(implicit c: Codec[A]): A = c.decode(value)

  implicit val stringCodec: Codec[String] =
    new Codec[String] {
      override def encode(value: String): String = value
      override def decode(value: String): String = value
    }
  implicit val intCodec: Codec[Int] =
    stringCodec.imap(_.toInt, _.toString)
  implicit val booleanCodec: Codec[Boolean] =
    stringCodec.imap(_.toBoolean, _.toString)
  implicit val doubleCodec: Codec[Double] =
    stringCodec.imap(_.toDouble, _.toString)
  implicit def boxCodec[A](implicit c: Codec[A]): Codec[Box[A]] =
    c.imap[Box[A]](Box(_), _.value)
  println(decode[Box[Double]]("1234"))



  import cats.Contravariant
  import cats.Show
  import cats.instances.string._
  import cats.instances.double._
  import cats.syntax.contravariant._
  val showString = Show[String]
  val showSymbol = Contravariant[Show].contramap(showString)((sym: Symbol) => s"${sym.name}")
  showSymbol.show(Symbol("dave"))

  val showDouble = Show[Double]
  val showBoxDouble = showDouble.contramap[Box[Double]](box => box.value * 10)
  println(showBoxDouble.show(Box(100.0)))

  import cats.Monoid
  import cats.syntax.invariant._
  import cats.syntax.semigroup._
  val monoidString = Monoid[String]
  implicit val symbolMonoid: Monoid[Symbol] = monoidString.imap(Symbol.apply)(_.name)
  Monoid[Symbol].empty
  val symbolResult = Symbol("a") |+| Symbol("few") |+| Symbol("words")
  println(symbolResult.name)
  val funcD1 = (x: Int) => x.toDouble
  val funcD2 = (y: Double) => y * 2

  val funcD3 = funcD1.map(funcD2)

}
