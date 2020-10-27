package rocktheJvm.part3datamanipulation

object Evaluation {
  /*
  *  Cats makes the distinction between
  *  - evaluating an expression eagerly
  *  - evaluating lazily and every time you request it
  *  - evaluating lazily and keeping the value (memoizing)
  * */
  import cats.Eval
  val instantEval: Eval[Int] = Eval.now {
    println("Computing now!")
    10
  }
  val redoEval: Eval[Int] = Eval.always {
    println("Computing again!")
    11
  }
  val delayedEval: Eval[Int] = Eval.later {
    println("Computing later!")
    12
  }

  val composedEvaluation =
    instantEval.flatMap( value1 =>
      delayedEval.map( value2 =>
        value1 + value2
      )
    )

  val anotherComposeEvaluation = for {
    value1 <- instantEval
    value2 <- delayedEval
  } yield value1 + value2

  // TODO 1:
  val evalEx1 = for {
    a <- delayedEval
    b <- redoEval
    c <- instantEval
    d <- redoEval
  } yield a + b + c + d

  val dontRecompute = redoEval.memoize
  val tutorial = Eval.always {
    println("Step 1 ...")
    "instructions for step 01"
  }.map { step1 =>
    println("Step 2")
    s"$step1 then instructions for step 02"
  }.memoize // above part will repeat once [it is later eval]
    .map { step12 => // this part will repeat again [it is always eval]
      println("Step 3, more complicated")
      s"$step12 then instructions for step 03"
    }

  def defer[T](eval: => Eval[T]): Eval[T] =
    Eval.later(()).flatMap(_ => eval)

  def reverseList[T](list: List[T]): List[T] = list match {
    case Nil => list
    case head :: tail => reverseList(tail) :+ head
  }

  def reverseEval[T](list: List[T]): Eval[List[T]] = list match {
    case head :: tail =>
        Eval.defer(reverseEval(tail).map(_ :+ head))
    case Nil => Eval.now(list)
  }


  def main(args: Array[String]): Unit = {
    defer(Eval.now {
      println("Now!!")
      42
    })
    println(reverseEval((1 to 10000).toList).value)
  }
}
