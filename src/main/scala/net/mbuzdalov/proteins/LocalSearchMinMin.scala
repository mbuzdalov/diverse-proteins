package net.mbuzdalov.proteins

import java.util.concurrent.ThreadLocalRandom

object LocalSearchMinMin:
  private class DistanceCache(cont: Container, selection: Array[Int], useMin: Boolean):
    private val distances = Array.tabulate(cont.size, selection.length):
      (i, j) => cont.manhattanDistance(i, selection(j))

    private def distFun(contIndex: Int, exceptArrayIndex: Int): Double =
      val slice = distances(contIndex)
      if useMin then
        Loops.mapMin(0, selection.length): i =>
          if i == exceptArrayIndex then Double.PositiveInfinity else slice(i)
      else
        Loops.mapSum(0, selection.length): i =>
          if i == exceptArrayIndex then 0.0 else slice(i)

    def tryImprove(index: Int): Boolean =
      var bestI = -1
      var bestV = distFun(selection(index), index)
      Loops.foreach(0, cont.size): i =>
        val currV = distFun(i, index)
        if currV > bestV then
          bestV = currV
          bestI = i
      if bestI == -1 then false else
        selection(index) = bestI
        Loops.foreach(0, cont.size): i =>
          distances(i)(index) = cont.manhattanDistance(i, bestI)
        true
  end DistanceCache

  def optimize(cont: Container, selectionSize: Int, useMin: Boolean): IArray[Int] =
    val rng = ThreadLocalRandom.current()
    val selectionBuilder = new scala.collection.mutable.HashSet[Int]()
    while selectionBuilder.size < selectionSize do
      selectionBuilder += rng.nextInt(cont.size)
    val selection = selectionBuilder.toArray
    val cache = new DistanceCache(cont, selection, useMin)
    val ordering = Array.tabulate(selectionSize)(identity)
    var countUntested = selectionSize
    while countUntested > 0 do
      val orderingIndex = rng.nextInt(countUntested)
      val realIndex = ordering(orderingIndex)

      if cache.tryImprove(realIndex) then
        // improved OK, reset the mutation set
        countUntested = selectionSize

      // if we failed to improve, we remove the element from the mutation set
      // if we did not, we also remove the element from the just-reset mutation set
      ordering(orderingIndex) = ordering(countUntested - 1)
      ordering(countUntested - 1) = realIndex
      countUntested -= 1

    IArray(selection *)
