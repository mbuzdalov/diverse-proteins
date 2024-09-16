package net.mbuzdalov.proteins

import java.util.concurrent.ThreadLocalRandom

object LocalSearchMinMin:
  private class DistanceCache(cont: Container, selection: Array[Int]):
    private val distances = Array.tabulate(cont.size, selection.length): 
      (i, j) => cont.manhattanDistance(i, selection(j))

    def minExcept(contIndex: Int, exceptArrayIndex: Int): Double =
      val slice = distances(contIndex)
      Loops.mapMin(0, selection.length): i =>
        if i == exceptArrayIndex then Double.PositiveInfinity else slice(i)
    
    def sumExcept(contIndex: Int, exceptArrayIndex: Int): Double =
      val slice = distances(contIndex)
      Loops.mapSum(0, selection.length): i =>
        if i == exceptArrayIndex then 0.0 else slice(i)
    
    def tryImprove(index: Int): Boolean =
      var best = -1
      var bestMin = minExcept(selection(index), index)
      Loops.foreach(0, cont.size): i =>
        val currMin = minExcept(i, index)
        if currMin > bestMin then
          bestMin = currMin
          best = i
      if best == -1 then false else
        selection(index) = best
        Loops.foreach(0, cont.size): i =>
          distances(i)(index) = cont.manhattanDistance(i, best)
        true
  end DistanceCache    
  
  def optimize(cont: Container, selectionSize: Int): Solution =
    val rng = ThreadLocalRandom.current()
    val selectionBuilder = new scala.collection.mutable.HashSet[Int]()
    while selectionBuilder.size < selectionSize do
      selectionBuilder += rng.nextInt(cont.size)
    val selection = selectionBuilder.toArray
    val cache = new DistanceCache(cont, selection)
    val ordering = Array.tabulate(selectionSize)(identity)
    var countUntested = selectionSize
    while countUntested > 0 do
      val orderingIndex = rng.nextInt(countUntested)
      val realIndex = ordering(orderingIndex)
      if cache.tryImprove(realIndex) then
        countUntested = selectionSize
      else  
        // failed to replace
        ordering(orderingIndex) = ordering(countUntested - 1)
        ordering(countUntested - 1) = realIndex
        countUntested -= 1

    val min = Loops.mapMin(0, selectionSize)(i => cache.minExcept(selection(i), i))
    val sumMin = Loops.mapSum(0, selectionSize)(i => cache.minExcept(selection(i), i))
    val sumSum = Loops.mapSum(0, selectionSize)(i => cache.sumExcept(selection(i), i)) / 2
    
    Solution(IArray(selection *), 
      Solution.NamedCost("min", min), 
      Solution.NamedCost("sum-min", sumMin), 
      Solution.NamedCost("sum-sum", sumSum))
