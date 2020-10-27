package part01.chapter04

object EvalCats {
  import cats.Eval
  val now = Eval.now(math.random + 1000)
  val always = Eval.now(math.random + 3000)
  val later = Eval.now(math.random + 2000)

  println(now.value)
  println(always.value)
  println(later.value)

  val greeting = Eval
    .always { println("Step 1"); "Hello" }
    .map {str => println("Step 2"); s"$str world"}

  def factorial(n: BigInt): Eval[BigInt] =
    if(n == 1) Eval.now(n)
    else
      Eval.defer(factorial(n - 1).map(_ * n))

  def foldRight[A, B](as: List[A], acc: B)(fn: (A,B) => B): B = as match {
    case head::tail =>
        fn(head,foldRight(tail, acc)(fn))
    case Nil => acc
  }

  def foldRightEval[A,B](as: List[A], acc: Eval[B])(fn: (A,Eval[B]) => Eval[B]): Eval[B] =
    as match {
      case head::tail =>
          Eval.defer(fn(head, foldRightEval(tail, acc)(fn)))
      case Nil => acc
    }

  def foldRightFunctional[A,B](as: List[A], acc: B)(fn:(A, B) => B): B =
    foldRightEval(as, Eval.now(acc)) { (a,b) => b.map(fn(a,_)) }.value

  foldRightFunctional((1 to 100000).toList, 0L)(_ + _)

}
