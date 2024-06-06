package net.mbuzdalov.proteins

object Distance:
  def manhattan(a: IArray[Float], b: IArray[Float]): Double =
    Loops.mapSum(0, a.length)(i => math.abs(a(i) - b(i)).toDouble)
