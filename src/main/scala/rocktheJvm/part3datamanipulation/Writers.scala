package rocktheJvm.part3datamanipulation

object Writers {
  import cats.data.Writer
  // 1 - define them at the start
  val aWriter: Writer[List[String], Int] = Writer(List("Started something"), 45)

  // 2 - manipulate them with pure fp
  val anIncreaseWriter = aWriter.map(_ + 1) // value increases, logs stay the same
  val aLogsWriter = aWriter.mapWritten(_ :+ "found something interesting")
  val aWriterWithBoth = aWriter.bimap(_:+ "found something interesting", _ + 1)
  val aWriterWithBoth2 = aWriter.mapBoth { (logs, value) =>
    (logs :+ "found something interesting", value + 1)
  }

  // flapMap
  import cats.instances.vector._
  val writerA = Writer(Vector("Log A1", "Log A2"), 10)
  val writerB = Writer(Vector("Log B1"), 40)
  val compositeWriter = for {
    va <- writerA
    vb <- writerB
  } yield va + vb

  // reset the logs
  import cats.instances.list._ // a Monoid[List[Int]]
  val anEmptyWriter = aWriter.reset


  // 3 - dump wither the value or logs
  val desireValue = aWriter.value
  val logs = aWriter.written
  val (l, v) = aWriter.run

  // TODO 1: rewrite a function which "prints" things with writers
  type Logged[A] = Writer[Vector[String], A]
  def countAndLog(n: Int): Logged[Int] =
    if(n <= 0) Writer(Vector("Starting"), 0)
    else countAndLog(n - 1).flatMap(_ => Writer(Vector(s"$n"), n))

  import cats.syntax.writer._
  import cats.syntax.applicative._

  import cats.instances.vector._
  def naiveSum(n: Int): Logged[Int] =
    for {
      sum <- if(n <= 0) 0.pure[Logged] else naiveSum(n - 1).mapBoth { (logs, value) =>
        (logs :+ s"Computed sum(${n - 1}) = $value",
          value + n)
      }
      _ <- Vector(s"Now at ${n}").tell
    } yield sum
  def main(args: Array[String]): Unit = {
    naiveSum(100).written.foreach(println)
  }
}
