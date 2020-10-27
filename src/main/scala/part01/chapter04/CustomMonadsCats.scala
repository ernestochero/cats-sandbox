package part01.chapter04

object CustomMonadsCats extends App {
  import cats.Monad
  import scala.annotation.tailrec

  val optionMonad = new Monad[Option] {
    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa flatMap f
    override def pure[A](x: A): Option[A] = Some(x)
    @tailrec
    override def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] =
      f(a) match {
        case None => None
        case Some(Left(a1)) => tailRecM(a1)(f)
        case Some(Right(b)) => Some(b)
      }
  }
  import cats.syntax.flatMap._
  // this method works with small data
  def retry[F[_]: Monad, A](start: A)(f: A => F[A]):F[A] =
    f(start).flatMap(a => retry(a)(f))

  import cats.instances.option._
  val r = retry[Option, Int](100)(a => if(a == 0) None else Some(a - 1))
  println(r)

  // rewrite the method above with tailRecM
  import cats.syntax.functor._
  def retryTailRecM[F[_]: Monad,A](start: A)(f: A => F[A]): F[A] =
    Monad[F].tailRecM(start) { a =>
      f(a).map(a2 => Left(a2))
    }
  val r1 = retryTailRecM[Option, Int](1000)(a => if (a == 0) None else Some(a - 1))
  println(r1)

  import cats.syntax.monad._
  def retryM[F[_]: Monad, A](start: A)(f: A => F[A]): F[A] =
    start.iterateWhileM(f)(_ => true)
  val r2 = retryM[Option, Int](10000)(a => if (a == 0) None else Some(a - 1))
  println(r2)

  // coding the monad Tree
  sealed trait Tree[+A]
  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  final case class Leaf[A](value: A) extends Tree[A]
  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)
  def leaf[A](value: A): Tree[A] = Leaf(value)
  implicit val treeMonad: Monad[Tree] = new Monad[Tree] {
    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = fa match {
      case Leaf(v) => f(v)
      case Branch(l,r) => Branch(flatMap(l)(f), flatMap(r)(f))
    }
    override def pure[A](x: A): Tree[A] = leaf(x)
    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = flatMap(f(a)) {
      case Left(value) => tailRecM(value)(f)
      case Right(value) => leaf(value)
    }
  }

  branch(leaf(100), leaf(200))
    .flatMap(x => branch(leaf(x - 1), leaf(x + 1)))
}
