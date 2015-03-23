/**
 * @author fairjm
 */
package cc.tsp

import cc.tsp.util.Configuration
import scala.collection.mutable.ListBuffer
import java.util.concurrent.{ ThreadLocalRandom => TLRandom }

case class Chromosome(dna: List[Int])

object Chromosome {
  import Configuration._
  lazy val cityList = (1 to citySize).toList

  /**
   * the crossOverPosition is in the 1/2 of the city size
   */
  val crossOverPosition = citySize / 2

  def generate = Chromosome(scala.util.Random.shuffle(cityList))

  /**
   * cross over the given chromosome and return the two new chromosomes
   * @param g1
   * @param g2
   *
   * @return (Chromosome, Chromosome)
   */
  def crossOver(c1: Chromosome, c2: Chromosome): (Chromosome, Chromosome) = {
    val newDna1 = ListBuffer[Int]()
    val newDna2 = ListBuffer[Int]()
    val (frontDna1, endDna1) = c1.dna.splitAt(crossOverPosition)
    val (frontDna2, endDna2) = c2.dna.splitAt(crossOverPosition)
    newDna1 ++= frontDna1 ++= endDna2
    newDna2 ++= frontDna2 ++= endDna1
    def distinctAndAppendRest(list: ListBuffer[Int]): List[Int] = {
      val distinctList = list.distinct
      distinctList ++= scala.util.Random.shuffle(cityList.diff(distinctList))
      distinctList.toList
    }
    (Chromosome(distinctAndAppendRest(newDna1)), Chromosome(distinctAndAppendRest(newDna2)))
  }

  /**
   * mutate the chromosome
   * @param g
   * @return Chromosome
   */
  def mutation(g: Chromosome): Chromosome = {
    val newDna = ListBuffer[Int]()
    newDna ++= g.dna
    val random = TLRandom.current()
    val p1 = random.nextInt(citySize)
    val p2 = {
      var tmp = random.nextInt(citySize)
      while (tmp == p1) {
        tmp = random.nextInt(citySize)
      }
      tmp
    }
    val v1 = newDna(p1)
    val v2 = newDna(p2)
    newDna.update(p1, v2)
    newDna.update(p2, v1)
    Chromosome(newDna.toList)
  }
}

case class Population(chromosomes: List[Chromosome], generationNum: Int = 0) {
  val populationSize = chromosomes.size
}

object Population {
  /**
   * use to init population
   * @param size
   * the size of population
   * @return Population
   */
  def generate(size: Int) = {
    Population(List.fill(size)(Chromosome.generate))
  }
}
