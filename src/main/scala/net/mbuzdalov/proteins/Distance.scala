package net.mbuzdalov.proteins

object Distance:
  def manhattan(a: IArray[Float], b: IArray[Float]): Double =
    var sum = 0.0
    var i = 0
    while i < a.length do
      sum += math.abs(a(i) - b(i))
      i += 1
    sum  
  end manhattan
  