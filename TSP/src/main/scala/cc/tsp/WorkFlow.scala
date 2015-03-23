package cc.tsp

import cc.tsp.util.Configuration._
import cc.tsp.util.Roulette
import scala.collection.mutable.ListBuffer
import java.util.concurrent.{ ThreadLocalRandom => TLRandom }
import cc.tsp.util.Distance
import scala.collection.mutable.ArrayBuffer

object WorkFlow {

  val keepedSize = {
    if (chromosomeSize < 10) 1
    else if (chromosomeSize <= 1000) 5
    else chromosomeSize / 200;
  }

  def iterate(lastPopulation: Population): Population = {
    if (lastPopulation.generationNum >= maxGenerationnNum) {
      lastPopulation
    } else {
      val fitness = Fitness.calculate(lastPopulation)
      fitness.take(10).foreach(r => println(r._2))
      fitness.take(10).foreach(r => println(Distance.loopDistance(r._1.dna)))
      // keep the chromosome that has the highest fitness
      println(lastPopulation.generationNum + " gen " + lastPopulation.chromosomes.distinct.size)
      println("=================")
      val keeped = fitness.take(keepedSize).map(_._1)

      val choosed = Roulette.choose(fitness, chromosomeSize - keepedSize)
      val t = System.currentTimeMillis()
      val crossOverPairNum = ((choosed.size * crossOverRate) / 2).toInt
      println("crossOverPairNum:" + crossOverPairNum)
      val crossOvered = crossOver(choosed, crossOverPairNum)
      println("time:" + (System.currentTimeMillis() - t))
      val mutationNum = (crossOvered.size * mutationRate).toInt
      val mutated = mutate(crossOvered, mutationNum)
      iterate(Population(keeped ::: mutated, lastPopulation.generationNum + 1))
    }
  }

  private def crossOver(cs: List[Chromosome], pairNum: Int): List[Chromosome] = {
    if (crossOverRate <= 0) {
      cs
    } else {
      val random = TLRandom.current()
      var csBuffer = ArrayBuffer[Chromosome]()
      val crossed = ArrayBuffer[Chromosome]()
      csBuffer ++= cs
      for (
        i <- 1 to pairNum if csBuffer.distinct.size > 1
      ) {
        var index1 = 0
        var index2 = 0
        while (csBuffer(index2) == csBuffer(index1)) {
          index1 = random.nextInt(csBuffer.size - 1)
          //keep index2 is larger than index1 that we can firstly remove index2 and keep the elem in index1 still
          index2 = random.nextInt(index1 + 1, csBuffer.size)
        }
        val c2 = csBuffer.remove(index2)
        val c1 = csBuffer.remove(index1)
        val (newC1, newC2) = Chromosome.crossOver(c1, c2)
        crossed += newC1 += newC2
      }
      csBuffer.toList ++ crossed.toList
    }
  }

  private def mutate(cs: List[Chromosome], mutationNum: Int): List[Chromosome] = {
    if (mutationNum <= 0) {
      cs
    } else {
      val random = TLRandom.current()
      val csBuffer = ListBuffer[Chromosome]()
      val mutated = ListBuffer[Chromosome]()
      csBuffer ++= cs
      for (
        i <- 1 to mutationNum if !csBuffer.isEmpty
      ) {
        val c = csBuffer.remove(random.nextInt(csBuffer.size))
        mutated += Chromosome.mutation(c)
      }
      csBuffer.toList ++ mutated.toList
    }
  }

  private def getBestInThePopulation(p: Population) = {
    val f = Fitness.calculate(p)
    f(0)._1
  }

  def main(args: Array[String]): Unit = {

    /**
     *  init population.population num is zero
     */
    val initPopulation = Population.generate(chromosomeSize)

    val resultPopulation = iterate(initPopulation)

    val resultChromosome = getBestInThePopulation(resultPopulation)

    val resultDna = resultChromosome.dna

    println("city size:" + resultDna.distinct.size)
    println(resultDna)
    println("loop distance:" + Distance.loopDistance(resultDna))
  }
}