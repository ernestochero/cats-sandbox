package part01.chapter07

object FoldableCats extends App {
  def show[A](list: List[A]): String =
    list.foldLeft("nil")( (accum, item) => s"$item then $accum"  )
  println(show(Nil))
  println(show(List(1, 2, 3)))
  List(1,2,3).foldLeft(List.empty[Int])( (a, i) => i :: a)

  import cats.Foldable
  import cats.instances.list._
  import cats.instances.option._
  val ints = List(1, 2, 3)
  val maybeInt = Option(1233)
  Foldable[List].foldLeft(ints, 0)(_ + _)
  Foldable[Option].foldLeft(maybeInt, 10)(_ * _)

  import cats.Eval
  import cats.Foldable
  def bigData = (1 to 100000).to(LazyList)
  println(bigData.foldRight(0L)(_ + _))
  import cats.instances.lazyList._
  val eval: Eval[Long] =
    Foldable[LazyList].foldRight(bigData, Eval.now(0L)){ (num, eval) =>
      eval.map(_ + num)
    }
  println(eval.value)
  import cats.instances.int._
  import cats.instances.string._
  val lst = List(1,2,3)
  Foldable[List].combineAll(lst)
  Foldable[List].foldMap(lst)(_.toString)
  println(Foldable[List].fold(lst))
  import cats.instances.vector._
  val values = List(Vector(1, 2, 3), Vector(4, 5, 6))
  (Foldable[List] compose Foldable[Vector]).combineAll(values)
  import cats.syntax.foldable._
  lst.combineAll
  lst.foldMap(_.toString)

  def sum[F[_]: Foldable](values: F[Int]): Int =
    values.foldLeft(1)(_ + _)

  println(sum(Option(10)))
  println(sum(lst))

}
