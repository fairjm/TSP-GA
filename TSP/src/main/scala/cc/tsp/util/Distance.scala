package cc.tsp.util

import scala.io.Source
import scala.util.Try
import scala.util.Failure
import scala.util.Success

object Distance {

  private val distances = collection.concurrent.TrieMap[(Int, Int), Int]()

  /**
   *  @param city1
   *  @param city2
   *  @return return 0 if city1 is equal to city2
   */
  def apply(city1: Int, city2: Int): Int = {
    if (city1 == city2) 0
    else if (city1 > city2) {
      distances.getOrElseUpdate((city2, city1), calculate(city2, city1))
    } else {
      distances.getOrElseUpdate((city1, city2), calculate(city1, city2))
    }
  }

  /**
   *  @param cities List(1,2,3,4) will calculate 1->2->3->4->1(a loop)
   *  @return return 0 if the size of cities is equal to or less than 1
   */
  def loopDistance(cities: List[Int]): Int = {
    if (cities.size <= 1) {
      0
    } else {
      (cities(0) :: cities.reverse).reverse.sliding(2).foldLeft(0) {
        (r, e) => r + apply(e(0), e(1))
      }
    }
  }

  private def calculate(city1: Int, city2: Int): Int = {
    import Configuration._
    val xy1 = xy(city1)
    val xy2 = xy(city2)
    Math.sqrt((xy1._1 - xy2._1) * (xy1._1 - xy2._1) + (xy1._2 - xy2._2) * (xy1._2 - xy2._2)).toInt
  }

}

