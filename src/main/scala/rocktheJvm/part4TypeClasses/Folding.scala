package rocktheJvm.part4TypeClasses

import cats.Eval
import part01.chapter02.MonoidSemigroup.Monoid

object Folding {
  // TODO - implement all in terms of foldLeft and foldRight
  object ListExercises {
    def map[A, B](list: List[A])(f: A => B): List[B] =
      list.foldLeft(List.empty[B])((listB, a) => f(a) :: listB)

    def flapMap[A, B](list: List[A])(f: A => List[B]): List[B] =
      list.foldLeft(List.empty[B])((listB, a) =>
        listB.foldRight(f(a))(_ :: _)
      ) // listB ++ f(a)
    def filter[A](list: List[A])(predicate: A => Boolean): List[A] =
      list.foldLeft(List.empty[A])((currentList, a) =>
        if (predicate(a)) a :: currentList else currentList
      )

    def combineAll[A](list: List[A])(implicit monoid: Monoid[A]): A =
      list.foldLeft(monoid.empty)(monoid.combine)
  }

  import cats.Foldable
  import cats.instances.list._
  import cats.instances.option._
  val sum: Int = Foldable[List].foldLeft(List(1, 2, 3), 0)(_ + _)
  val sumOption: Int = Foldable[Option].foldLeft(Option(2), 3)(_ + _) // 5
  // foldRight is stack-safe regardless of your container
  val sumRight: Eval[Int] =
    Foldable[List].foldRight(List(1, 2, 3), Eval.now(0)) { (num, eval) =>
      eval.map(_ + num)
    }
  import cats.instances.int._ // Monoid[Int]
  val anotherSum: Int = Foldable[List].combineAll(List(1, 2, 3))
  val mappedConcat: Int = Foldable[List].foldMap(List(1, 2, 3))(_ + 10)
  val intNested = List(Vector(1, 2, 3), Vector(4, 5, 6))
  import cats.instances.vector._
  Foldable[List].compose(Foldable[Vector]).combineAll(intNested)

  def main(args: Array[String]): Unit = {
    import ListExercises._
    val numbers = (1 to 10).toList
    println(flapMap(numbers)(x => (1 to x).toList))
  }
}
