package net.mbuzdalov.proteins

import java.util.concurrent.ThreadLocalRandom

object LocalSearchMinMin:
  def optimize(cont: Container, selectionSize: Int): Solution =
    val rng = ThreadLocalRandom.current()
    val selection = new scala.collection.mutable.HashSet[Int]()
    while selection.size < selectionSize do
      selection += rng.nextInt(cont.size)
    val fixed = IArray(selection.toArray *)
    optimize(cont, fixed, cont.evaluateFromScratch(fixed *))

  private def chooseOther(cont: Container, array: Array[Int], index: Int, currentDistance: Double): Int =
    var result = -1
    var bestDistance = currentDistance
    Loops.foreach(0, cont.size): i =>
      if i != array(index) then
        val minOthers = Loops.mapMin(0, array.length): j =>
          if j == index then Double.PositiveInfinity else cont.manhattanDistance(array(j), i)
        if minOthers > bestDistance then
          bestDistance = minOthers
          result = i
    result

  private def replace(cont: Container, array: Array[Int], index: Int, newValue: Int, matrix: Array[Array[Double]]): Unit =
    array(index) = newValue
    Loops.foreach(0, array.length): i =>
      if i != index then
        matrix(i)(index) = cont.manhattanDistance(array(i), newValue)
        matrix(index)(i) = matrix(i)(index)

  private def optimize(cont: Container, initialIndices: IArray[Int], initialCost: Double): Solution =
    val n = initialIndices.length
    val rng = ThreadLocalRandom.current()
    val current = Array(initialIndices *)
    val distanceMatrix = Array.tabulate(n, n)((i, j) => if i == j then Double.PositiveInfinity else cont.manhattanDistance(current(i), current(j)))
    val ordering = Array.tabulate(n)(identity)
    var countUntested = n
    while countUntested > 0 do
      val orderingIndex = rng.nextInt(countUntested)
      val realIndex = ordering(orderingIndex)
      val otherIndex = chooseOther(cont, current, realIndex, distanceMatrix(realIndex).min)
      if otherIndex == -1 then
        // failed to replace
        ordering(orderingIndex) = ordering(countUntested - 1)
        ordering(countUntested - 1) = realIndex
        countUntested -= 1
      else
        // updated, need to recompute
        countUntested = n
        replace(cont, current, realIndex, otherIndex, distanceMatrix)

    val min = Loops.mapMin(0, n)(i => distanceMatrix(i).min)
    val sumMin = Loops.mapSum(0, n)(i => distanceMatrix(i).min)
    val sumSum = Loops.mapSum(0, n)(i => distanceMatrix(i).filter(_.isFinite).sum) / 2
    
    Solution(IArray(current *), 
      Solution.NamedCost("min", min), 
      Solution.NamedCost("sum-min", sumMin), 
      Solution.NamedCost("sum-sum", sumSum))
