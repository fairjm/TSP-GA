package cc.tsp.util

import scala.util.Failure
import scala.util.Success
import scala.util.Try
import scala.io.Source

object Configuration {
  lazy val xy: Map[Int, (Int, Int)] = {
    withResource("resource/main/scala/att48.tsp") {
      source =>
        source.getLines().toList.map(_.split(" ")).foldLeft(Map[Int, (Int, Int)]()) {
          (r, e) =>
            r + (Integer.valueOf(e(0)).toInt -> (Integer.valueOf(e(1)), Integer.valueOf(e(2))))
        }
    }
  }

  lazy val citySize = xy.size

  val crossOverRate = 0.8

  val mutationRate = 0.01

  val chromosomeSize = 1000

  val maxGenerationnNum = 10000

  def withResource[T](fileName: String)(op: Source => T): T = {
    val s = Try(Source.fromFile(fileName))
    s match {
      case Failure(e) => throw e
      case Success(source) => try {
        op(source)
      } finally {
        if (s.isSuccess) {
          s.get.close()
        }
      }
    }
  }
}