package rocktheJvm.part2abstractMath

object Functors {
  // simplified definition
  trait MyFunctor[F[_]] {
    def map[A,B](fa: F[A])(f: A => B):F[B]
  }

  import cats.Functor
  import cats.instances.list._
  import cats.instances.option._
  import cats.instances.try_._

  import scala.util.Try
  val functorList: List[Int] = Functor[List].map(List(1,2,3))(_ + 1) // List(2,3,4)
  val functorOption: Option[Int] = Functor[Option].map(Option(2))(_ + 1) // Some(3)
  val functorTry: Try[Int] = Functor[Try].map(Try(2))(_ + 1) // Success(3)

  // generalizing an API
  import cats.Semigroup
  import cats.instances.int._
  import cats.syntax.semigroup._
  def do10xList(list: List[Int]): List[Int] = list.map(_ * 10)
  def do10x[F[_]](container: F[Int])(implicit functor: Functor[F]): F[Int] =
    functor.map(container)(_ * 10)

  def doxIncrement[F[_], T: Semigroup](container: F[T], increment: T)(implicit functor: Functor[F]): F[T] =
    functor.map(container)(_ |+| increment)

  // TODO 1: define your own functor for a binary tree
  // hint: define an object which extends Functor[Tree]
  trait Tree[+T]
  case class Leaf[+T](value: T) extends Tree[T]
  case class Branch[+T](value: T , left: Tree[T], right: Tree[T]) extends Tree[T]
  implicit object treeFunctor extends Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] =
      fa match {
        case Leaf(v) => Leaf(f(v))
        case Branch(v, left, right) =>
          Branch(f(v), map(left)(f), map(right)(f))
      }
  }
  object Tree {
    // "smart" constructors
    def branch[T](value: T, left: Tree[T], right: Tree[T]): Tree[T] = Branch(value, left, right)
    def leaf[T](value: T): Tree[T] = Leaf(value)
  }

  import Tree._
  val tree: Tree[Int] = branch(30, Leaf(20), Leaf(10))

  // extension method - map
  import cats.syntax.functor._
  val tree01: Tree[Int] = branch(40, branch(5, leaf(30), leaf(20)), leaf(10))
  val incrementedTree: Tree[Int] = tree01.map(_ + 1)

  // TODO 2: write a shorter do10x method using extension methods
  def do10xShorter[F[_]: Functor](container: F[Int]): F[Int] =
    container.map(_ * 10)

  def main(args: Array[String]): Unit = {
    println(do10x(List(2,3,4)))
    println(do10x(Option(2)))
    println(do10x(Try(35)))
    println(doxIncrement(Option(10), 20)) // Some(30)
    println(doxIncrement(List(5, 10), 20)) // List(25, 30)
    println(doxIncrement(Try(20), 50)) // Success(70)
    //
    println(do10x(branch(30, leaf(20), leaf(10))))
    println(do10x(tree))
    println(incrementedTree)
    println(do10xShorter(List(2,3,4)))
  }
}
