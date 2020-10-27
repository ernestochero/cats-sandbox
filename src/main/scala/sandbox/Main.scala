package sandbox

import cats.instances.string._
import cats.syntax.semigroup._

object Main extends App {
  println("Hello " |+| "Cats!")
  val data : Map[String, Job] = Map.empty[String, Job]
  case class Job(name: String, next: String)
  // if -> (a,bcd) => List((a,b),(a,c),(a,d))
  // if -> decodeStructure(b,xyx) => List((a,b),(b,x), (b,y), (b,z), (a,c), (a,d))
  def decodeStructure(name: String, next: String): Array[(String, String)] = {
      next.split(",").map((name, _))
  }

  def getJob(name: String): Job = data(name)
  def hasNext(next: String): Boolean = next != "#N/A" && next.split(",").length > 0

  def decode(job: Job, result: Array[(String, String)]): Array[(String, String)] = {
    if(!hasNext(job.next)) result
    else {
      val arr = decodeStructure(job.name, job.next)
      arr.foldLeft(result ++ arr) {
        case (acc,(_, next)) => decode(getJob(next),acc)
      }
    }
  }



}

