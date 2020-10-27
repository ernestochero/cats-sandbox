package part02.chapter09

object MapReduceCats extends App {
  // implementing foldMap
  import cats.Monoid
  import cats.syntax.semigroup._ // for |+| <==> combine
  def foldMap[A, B: Monoid](as: Vector[A])(func: A => B): B =
    as.foldLeft(Monoid[B].empty)(_ |+| func(_))
  import cats.instances.int._
  foldMap(Vector(1,2,3))(identity) // 6
  import cats.instances.string._
  foldMap(Vector(1,2,3))(_.toString + "! ")
  foldMap("Hello world!".toVector)(_.toString.toUpperCase)

  // Parallelising foldMap
  import scala.concurrent.Future
  import scala.concurrent.ExecutionContext.Implicits.global
  val future1 = Future {
    (1 to 100).toList.sum
  }
  val future2 = Future {
    (100 to 200).toList.sum
  }
  val future3 = future1.map(_.toString)

  val future4 = for {
    a <- future1
    b <- future2
  } yield a + b

  Future.sequence(List(Future(1), Future(2), Future(3)))
  import cats.instances.future._
  import cats.instances.list._
  import cats.syntax.traverse._
  List(Future(1), Future(2), Future(3)).sequence

  import cats.{Monoid, Monad}
  import cats.instances.int._
  Monad[Future].pure(42)
  Monoid[Future[Int]].combine(Future(1), Future(2))

  // divide work
  println(Runtime.getRuntime.availableProcessors())
  (1 to 10).toList.grouped(3).toList

  import cats.syntax.foldable._
  import cats.syntax.traverse._
  import cats.instances.vector._
  // implementing the parallel fold map
  def parallelFoldMap[A, B: Monoid]
    (values: Vector[A])(func: A => B) = {
    val numCores = Runtime.getRuntime.availableProcessors()
    val groupSize = (1.0 * values.size/numCores).ceil.toInt

    // create one group for each CPU
    val groups: Vector[Vector[A]] =
      values.grouped(groupSize).toVector

    // create a future to foldMap each group
    val traversed = groups.traverse(group => Future(group.foldMap(func)))
    traversed.map(_.combineAll)
  }

  val result: Future[Int] = parallelFoldMap((1 to 1000000).toVector)(identity)

  import scala.concurrent.Await
  import scala.concurrent.duration._
  println(Await.result(result, 1.second))
}
