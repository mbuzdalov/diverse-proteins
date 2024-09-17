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
    recombiner.go(0, Double.PositiveInfinity, 0, 0)
    println(f"    Time spent in brute-force: ${(System.nanoTime() - t1) * 1e-9}%.02f seconds")
    val solution = indices.indices.filter(i => recombiner.currentBestMask(i)).map(indices)
    val checkup = cont.evaluateFromScratch(solution *)
    assert(checkup == recombiner.bestMin)
    IArray(solution *)

class MegaRecombiner private(m: IArray[IArray[Double]], count: Int):
  private var bestMin, bestMinSum = 0.0
  private var currentBestMask = new MuBitSet()
  private val currMask = new MuBitSet()
  private val depthStack = Array.fill(m.length, m.length + 1)(Double.PositiveInfinity)
  
  def go(index: Int, prefixMin: Double, prefixMinSum: Double, depth: Int): Unit =
    //assert(prefixMin > bestMin || prefixMin == bestMin && prefixMinSum > bestMinSum)
    val prefixMinima = depthStack(depth)
    assert(prefixMin >= bestMin)
    val nBits = currMask.size
    if nBits == count then
      if prefixMin > bestMin || prefixMin == bestMin && prefixMinSum > bestMinSum then
        bestMin = prefixMin
        bestMinSum = prefixMinSum
        currentBestMask = currMask.clone()
        println(s"Updated to ($bestMin, $bestMinSum) by $currentBestMask")
    else
      assert(index < m.length)
      if m.length - index - 1 >= count - nBits then
        go(index + 1, prefixMin, prefixMinSum, depth)
      //if prefixMin > bestMin || prefixMin == bestMin && prefixMinSum > bestMinSum then
      if prefixMin >= bestMin then
        var updatedMin = Double.PositiveInfinity
        var updatedMinSum = 0.0
        Loops.foreach(0, index): i =>
          if currMask(i) then
            val v = m(i)(index)
            updatedMin = math.min(updatedMin, v)
            updatedMinSum += math.min(prefixMinima(i), v)
        updatedMinSum += updatedMin // the row that comes with the index being added
        //if updatedMin > bestMin || updatedMin == bestMin && updatedMinSum > bestMinSum then
        if updatedMin >= bestMin then
          val next = depthStack(depth + 1)
          System.arraycopy(prefixMinima, 0, next, 0, prefixMinima.length)
          Loops.foreach(0, index): i =>
            if currMask(i) then
              val d = m(i)(index)
              next(i) = math.min(next(i), d)
              next(index) = math.min(next(index), d)
          currMask.addOne(index)
          go(index + 1, math.min(prefixMin, updatedMin), updatedMinSum, depth + 1)
          currMask.remove(index)
