package net.mbuzdalov.proteins

object Distance:
  val Manhattan: Distance = (a, b) =>
    Loops.mapSum(0, a.length)(i => math.abs(a(i) - b(i)).toDouble)

  val Euclidean: Distance = (a, b) =>
    val sumSq = Loops.mapSum(0, a.length): i =>
      val diff = a(i).toDouble - b(i).toDouble
      diff * diff
    math.sqrt(sumSq)

  val Cosine: Distance = (a, b) =>
    var sumAA, sumBB, sumAB = 0.0
    Loops.foreach(0, a.length): i =>
      val ai: Double = a(i)
      val bi: Double = b(i)
      sumAA += ai * ai
      sumBB += bi * bi
      sumAB += ai * bi
    1 - sumAB / (math.sqrt(sumAA) * math.sqrt(sumBB))

  def manhattan(a: IArray[Float], b: IArray[Float]): Double =
    Loops.mapSum(0, a.length)(i => math.abs(a(i) - b(i)).toDouble)

trait Distance:
  def apply(a: IArray[Float], b: IArray[Float]): Double
