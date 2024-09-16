package net.mbuzdalov.proteins

import java.util.concurrent.ThreadLocalRandom

object LocalSearchMinSum:
  private def bothFromScratch(cont: Container, idx: IArray[Int]): (Double, Double) =
    var minDist = Double.PositiveInfinity
    var sumDist = 0.0
    Loops.foreach(0, idx.length): i =>
      Loops.foreach(i + 1, idx.length): j =>
        val dist = cont.manhattanDistance(idx(i), idx(j))
        minDist = math.min(minDist, dist)
        sumDist += dist
    (minDist, sumDist)

  private def bothFromMatrix(mx: Array[Array[Double]]): (Double, Double) =
    val n = mx.length
    var min = Double.PositiveInfinity
    var sum = 0.0
    Loops.foreach(0, n): i =>
      Loops.foreach(i + 1, n): j =>
        min = math.min(min, mx(i)(j))
        sum += mx(i)(j)
    (min, sum)

  private def bothForRow(idx: Int, row: Array[Double]): (Double, Double) =
    var min = Double.PositiveInfinity
    var sum = 0.0
    Loops.foreach(0, row.length): i =>
      if i != idx then
        min = math.min(min, row(i))
        sum += row(i)
    (min, sum)

  def optimize(cont: Container, selectionSize: Int): Solution =
    val rng = ThreadLocalRandom.current()
    val selection = new scala.collection.mutable.HashSet[Int]()
    while selection.size < selectionSize do
      selection += rng.nextInt(cont.size)
    val fixed = IArray(selection.toArray *)
    val (minDist, sumDist) = bothFromScratch(cont, fixed)
    optimize(cont, fixed, minDist, sumDist)

  private def chooseOther(cont: Container, array: Array[Int], index: Int, currentMin: Double, currentSum: Double): Int =
    var result = -1
    var bestMin = currentMin
    var bestSum = currentSum
    Loops.foreach(0, cont.size): i =>
      if i != array(index) then
        var minOthers = Double.PositiveInfinity
        var sumOthers = 0.0
        Loops.foreach(0, array.length): j =>
          if j != index then
            val d = cont.manhattanDistance(array(j), i)
            minOthers = math.min(minOthers, d)
            sumOthers += d

        if minOthers > bestMin || minOthers == bestMin && sumOthers > bestSum then
          bestMin = minOthers
          bestSum = sumOthers
          result = i
    result

  private def replace(cont: Container, array: Array[Int], index: Int, newValue: Int, matrix: Array[Array[Double]]): Unit =
    array(index) = newValue
    Loops.foreach(0, array.length): i =>
      if i != index then
        matrix(i)(index) = cont.manhattanDistance(array(i), newValue)
        matrix(index)(i) = matrix(i)(index)

  private def optimize(cont: Container, initialIndices: IArray[Int], initialMin: Double, initialSum: Double): Solution =
    val n = initialIndices.length
    val rng = ThreadLocalRandom.current()
    val current = Array(initialIndices *)
    val distanceMatrix = Array.tabulate(n, n)((i, j) => if i == j then Double.PositiveInfinity else cont.manhattanDistance(current(i), current(j)))
    val ordering = Array.tabulate(n)(identity)
    var countUntested = n
    while countUntested > 0 do
      val orderingIndex = rng.nextInt(countUntested)
      val realIndex = ordering(orderingIndex)
      val (rowMin, rowSum) = bothForRow(realIndex, distanceMatrix(realIndex))
      val otherIndex = chooseOther(cont, current, realIndex, rowMin, rowSum)
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
    val sumSum = Loops.mapSum(0, n)(i => distanceMatrix(i).sum) / 2

    Solution(IArray(current *),
      Solution.NamedCost("min", min),
      Solution.NamedCost("sum-min", sumMin),
      Solution.NamedCost("sum-sum", sumSum))
