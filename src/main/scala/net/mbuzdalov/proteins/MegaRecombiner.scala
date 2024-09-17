package net.mbuzdalov.proteins

import scala.collection.mutable.{BitSet => MuBitSet}

object MegaRecombiner:
  def recombine(cont: Container, count: Int, proteins: String): IArray[Int] =
    val indices = proteins.split(',').map(cont.index).distinct
    println(s"Number of proteins to consider: ${indices.length}")
    println(s"    Proteins: ${indices.map(cont.name).mkString(",")}")
    require(indices.length > count)

    val t0 = System.nanoTime()
    val matrix = IArray.tabulate(indices.length, indices.length):
      (i, j) => cont.manhattanDistance(indices(i), indices(j))
    println(f"    Time spent in matrix precalc: ${(System.nanoTime() - t0) * 1e-9}%.02f seconds")

    val recombiner = new MegaRecombiner(matrix, count)
    val t1 = System.nanoTime()
    recombiner.go(0, new MuBitSet(), Double.PositiveInfinity, 0, Array.fill(indices.length)(Double.PositiveInfinity))
    println(f"    Time spent in brute-force: ${(System.nanoTime() - t1) * 1e-9}%.02f seconds")
    val solution = indices.indices.filter(i => recombiner.currentBestMask(i)).map(indices)
    val checkup = cont.evaluateFromScratch(solution *)
    assert(checkup == recombiner.bestMin)
    IArray(solution *)

class MegaRecombiner private(m: IArray[IArray[Double]], count: Int):
  private var bestMin, bestMinSum = 0.0
  private var currentBestMask = new MuBitSet()
  def go(index: Int, mask: MuBitSet, prefixMin: Double, prefixMinSum: Double, prefixMinima: Array[Double]): Unit =
    //assert(prefixMin > bestMin || prefixMin == bestMin && prefixMinSum > bestMinSum)
    assert(prefixMin >= bestMin)
    val nBits = mask.size
    if nBits == count then
      if prefixMin > bestMin || prefixMin == bestMin && prefixMinSum > bestMinSum then
        bestMin = prefixMin
        bestMinSum = prefixMinSum
        currentBestMask = mask.clone()
        println(s"Updated to ($bestMin, $bestMinSum) by $currentBestMask")
    else
      assert(index < m.length)
      if m.length - index - 1 >= count - nBits then
        go(index + 1, mask, prefixMin, prefixMinSum, prefixMinima)
      //if prefixMin > bestMin || prefixMin == bestMin && prefixMinSum > bestMinSum then
      if prefixMin >= bestMin then
        var updatedMin = Double.PositiveInfinity
        var updatedMinSum = 0.0
        Loops.foreach(0, index): i =>
          if mask(i) then
            val v = m(i)(index)
            updatedMin = math.min(updatedMin, v)
            updatedMinSum += math.min(prefixMinima(i), v)
        updatedMinSum += updatedMin // the row that comes with the index being added
        //if updatedMin > bestMin || updatedMin == bestMin && updatedMinSum > bestMinSum then
        if updatedMin >= bestMin then
          val copy = prefixMinima.clone()
          Loops.foreach(0, index): i =>
            if mask(i) then
              val d = m(i)(index)
              copy(i) = math.min(copy(i), d)
              copy(index) = math.min(copy(index), d)
          mask.addOne(index)
          go(index + 1, mask, math.min(prefixMin, updatedMin), updatedMinSum, copy)
          mask.remove(index)
