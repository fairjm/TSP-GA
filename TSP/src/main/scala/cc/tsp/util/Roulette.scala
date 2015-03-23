/**
 * @author fairjm
 */
package cc.tsp.util

import cc.tsp.Chromosome
import java.util.concurrent.{ ThreadLocalRandom => TLRandom }
import scala.collection.mutable.ListBuffer
import cc.tsp.Population
import cc.tsp.Fitness

/**
 * used to choose Chromosome by fitness using Roulette
 */
object Roulette {

  /**
   * @param fitness the result of Fitness will be used here
   * @param chooseNum the num to be choosed
   * @return the choosed chromosome
   */
  def choose(fitness: List[(Chromosome, Double)], chooseNum: Int): List[Chromosome] = {
    val sum = fitness.map(_._2).sum
    val rates = fitness.map(_._2 / sum)
    // use an acc is not elegant "(/""≡ _ ≡)/~┴┴"
    var acc: Double = 0
    // calculate acc rate with index
    // function map should be side-effect free but the acc is out of its bound
    // so the function used in map blow is side-effect(it sucks)
    val accRatesWithIndex = rates.map { rate => acc = acc + rate; acc }.zipWithIndex
    val choosed = ListBuffer[Chromosome]()
    val random = TLRandom.current()
    for (i <- 1 to chooseNum) {
      val r = random.nextDouble()
      // get the list which rate is larger than or equal to r([0,1))
      val tmp = accRatesWithIndex.dropWhile(p => p._1 < r)
      val chooseIndex = if (tmp.isEmpty) {
        // choose the last if the r is larger than all(the last acc rate may not be 1)
        accRatesWithIndex(accRatesWithIndex.size - 1)._2
      } else {
        tmp(0)._2
      }
      choosed += fitness(chooseIndex)._1
    }
    choosed.toList
  }
}