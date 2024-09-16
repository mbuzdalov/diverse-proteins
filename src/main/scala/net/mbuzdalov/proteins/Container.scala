package net.mbuzdalov.proteins

class Container(seq: IArray[(String, IArray[Float])]):
  private val indexMap = seq.indices.map(i => (seq(i)._1, i)).toMap

  def size: Int = seq.length
  def name(index: Int): String = seq(index)._1
  def index(name: String): Int = indexMap(name)

  private def embedding(index: Int): IArray[Float] = seq(index)._2
  private def embedding(name: String): IArray[Float] = embedding(indexMap(name))

  def evaluateFromScratch(indices: Int*): Double =
    Loops.mapMin(0, indices.length): i =>
      Loops.mapMin(i + 1, indices.length): j =>
        manhattanDistance(indices(i), indices(j))

  def manhattanDistance(i1: Int, i2: Int): Double =
    Distance.manhattan(embedding(i1), embedding(i2))

  def explainSolution(solution: IArray[Int]): String =
    val n = solution.length

    // Compute fitness values: min, sum-min and sum-sum
    var min = Double.PositiveInfinity
    var sumMin, sumSum = 0.0
    Loops.foreach(0, n): i =>
      var rowMin = Double.PositiveInfinity
      Loops.foreach(0, n): j =>
        if i != j then
          val dist = manhattanDistance(solution(i), solution(j)) // could be cached, but this is not too much
          sumSum += dist
          rowMin = math.min(rowMin, dist)
      sumMin += rowMin
      min = math.min(min, rowMin)

    // Write out protein names
    val proteinNames = solution.map(this.name).sorted

    // Return the string
    s"Fitness [min = $min, sum-min = $sumMin, sum-sum = ${sumSum / 2}], proteins ${proteinNames.mkString(", ")}"
