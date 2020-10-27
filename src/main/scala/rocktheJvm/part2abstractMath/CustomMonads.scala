package rocktheJvm.part2abstractMath

import scala.annotation.tailrec

object CustomMonads {
  import cats.Monad
  implicit object OptionMonad extends Monad[Option] {
    override def pure[A](x: A): Option[A] = Option(x)
    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] =
      fa.flatMap(f)
    @tailrec
    override def tailRecM[A, B](a: A)(fa: A => Option[Either[A, B]]): Option[B] = fa(a) match {
      case None => None
      case Some(Left(a)) => tailRecM(a)(fa)
      case Some(Right(b)) => Some(b)
    }
  }

  // TODO 1: define a monad for the identity type
  type Identity[T] = T
  import cats.Id
  implicit object IdentityMonad extends Monad[Identity] {
    override def pure[A](x: A): Identity[A] = x
    override def flatMap[A, B](fa: Identity[A])(f: A => Identity[B]): Identity[B] = f(fa)
    @tailrec
    override def tailRecM[A, B](a: A)(f: A => Identity[Either[A, B]]): Identity[B] = f(a) match {
      case Left(a1) => tailRecM(a1)(f)
      case Right(b) => b
    }
  }

  sealed trait Tree[+A]
  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  final case class Leaf[A](value: A) extends Tree[A]
  object Tree {
    def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)
    def leaf[A](value: A): Tree[A] = Leaf(value)
  }
  // TODO 2: define a monad for this tree
  implicit object TreeMonad extends Monad[Tree] {
    import Tree._
    override def pure[A](x: A): Tree[A] = leaf(x)
    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = fa match {
      case Leaf(a) => f(a)
      case Branch(left, right) =>
          branch(flatMap(left)(f), flatMap(right)(f))
    }
    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = flatMap(f(a)) {
      case Left(a) => tailRecM(a)(f)
      case Right(b) => leaf(b)
    }
  }


  def main(args: Array[String]): Unit = {
    import Tree._
    import cats.syntax.flatMap._
    val tree: Tree[Int] = branch(leaf(10), leaf(20))
    val changedTree = tree.flatMap(x => branch(leaf(x + 1), leaf(x + 2)))
    println(changedTree)
  }
}
