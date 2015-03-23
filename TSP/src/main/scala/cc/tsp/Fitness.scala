package cc.tsp

import cc.tsp.util.Distance

object Fitness {

  private val factor = 10e5

  /**
   *
   * @param pop the population to calculate the fitness
   * @return the fiteness of each gene in the population,order by fitness from high to low
   *
   */
  def calculate(pop: Population): List[(Chromosome, Double)] = {
    pop.chromosomes.map { ch => (ch, factor / (Distance.loopDistance(ch.dna))) }.sortBy(-1 * _._2)
  }

}