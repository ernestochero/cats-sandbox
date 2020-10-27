package part01.chapter04

import cats.Eval
import cats.data.IndexedStateT

object StateMonadCats extends App {
  import cats.data.State
  val a: State[Int, String] = State[Int, String] { state => (state, s"The state is $state")}
  val (state, result) = a.run(10).value
  val justTheState: Int = a.runS(10).value
  val justTheResult: String = a.runA(10).value

  // now I am going to combine state monads
  val step1 = State[Int, String] { num =>
    val ans = num + 1
    (ans, s"Result of step1: $ans")
  }

  val step2 = State[Int, String] { num =>
    val ans = num * 2
    (ans, s"Result of step2: $ans")
  }

  val both: IndexedStateT[Eval, Int, Int, (String, String)] =
    for {
      s1 <- step1
      _ = println(s1)
      s2 <- step2
    } yield (s1, s2)

  val (s, r) = both.run(20).value
  println(s" $s and $r")

  // ways to work with state monads
  val getDemo = State.get[Int]
  getDemo.run(10).value // res1: (Int, Int) = (10, 10)

  val setDemo = State.set[Int](30)
  setDemo.run(10).value // res1: (Int, Unit) = (30, ())

  val pureDemo = State.pure[Int, String]("Result")
  pureDemo.run(10).value // res1: (Int, String) = (10, "Result")

  val inspectDemo = State.inspect[Int, String](num => s"this is $num")
  inspectDemo.run(10) // res1: (Int, String) = (10, "this is 10")

  val modifyDemo = State.modify[Int](_ + 1)
  modifyDemo.run(10) // res1: (Int, Unit) = (11, ())

  // complete program using state monads
  val program: State[Int, (Int, Int, Int)] = for {
    a <- State.get[Int]
    _ <- State.set(a + 1)
    b <- State.get[Int]
    _ <- State.modify[Int](_ + 1)
    c <- State.inspect[Int, Int](_ * 1000)
  } yield (a, b, c)
  val (st, re) = program.run(1).value
  println(s" $st and $re ")

  // implement a post order calculation
  type CalcState[A] = State[List[Int], A]
  def operand(num: Int): CalcState[Int] =
    State[List[Int], Int] { stack =>
      (num :: stack, num)
    }
  def operator(f: (Int, Int) => Int): CalcState[Int] =
    State[List[Int], Int] {
      case b :: a :: tail =>
        val ans = f(a, b)
        (ans :: tail, ans)
      case _ => sys.error("Fail!")
    }

  def evalOne(sym: String): CalcState[Int] =
    sym match {
      case "+" => operator(_ + _)
      case "-" => operator(_ - _)
      case "*" => operator(_ * _)
      case "/" => operator(_ / _)
      case num => operand(num.toInt)

    }
  println(evalOne("42").run(Nil).value)

  val p1 = for {
    _ <- evalOne("1")
    _ <- evalOne("2")
    ans <- evalOne("+")
  } yield ans
  println(p1.runA(Nil).value)

  import cats.syntax.applicative._
  def evalAll(input: List[String]): CalcState[Int] =
    input.foldLeft(0.pure[CalcState]) { (r,v) =>
      r.flatMap(_ => evalOne(v))
    }
  val multistageProgram = evalAll(List("1", "2", "+", "3", "*"))
  println(multistageProgram.runA(Nil).value)

  val biggerProgram = for {
    _ <- evalAll(List("1", "2", "+"))
    _ <- evalAll(List("3", "4", "+"))
    ans <- evalOne("*")
  } yield ans
  println(biggerProgram.runA(Nil).value)

  def evalInput(input: String): Int =
    evalAll(input.split(" ").toList).runA(Nil).value
  println(evalInput("1 2 + 3 4 +"))
}

object Ex extends App {
  import java.time.LocalDate
  import scala.util.chaining._
  val f = (x:Int) => x + 1
  val g = (x:Int) => x +  2
  val show = (x: Int) => {println(s"final result is : $x")}
  def increment(n: Int): Unit = n.pipe(f).pipe(g).pipe(show)
  def filterDate(dates: List[LocalDate], start: LocalDate, endDate: LocalDate): List[LocalDate] =
    dates.filter(ld => ld.isAfter(start) && ld.isBefore(endDate))
}