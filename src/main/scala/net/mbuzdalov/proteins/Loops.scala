package net.mbuzdalov.proteins

object Loops:
  inline def foreach(from: Int, until: Int)(inline fun: Int => Any): Unit =
    var i = from
    while i < until do
      fun(i)
      i += 1
  end foreach

  inline def mapSum(from: Int, until: Int)(inline fun: Int => Int): Int =
    var i = from
    var sum = 0
    while i < until do
      sum += fun(i)
      i += 1
    sum
  end mapSum

  inline def mapSum(from: Int, until: Int)(inline fun: Int => Long): Long =
    var i = from
    var sum = 0L
    while i < until do
      sum += fun(i)
      i += 1
    sum
  end mapSum

  inline def mapSum(from: Int, until: Int)(inline fun: Int => Double): Double =
    var i = from
    var sum = 0.0
    while i < until do
      sum += fun(i)
      i += 1
    sum
  end mapSum

  inline def mapMin(from: Int, until: Int)(inline fun: Int => Double): Double =
    var i = from
    var min = Double.PositiveInfinity
    while i < until do
      min = math.min(min, fun(i))
      i += 1
    min
  end mapMin
end Loops
