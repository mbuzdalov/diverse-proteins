package net.mbuzdalov.proteins

object MegaRecombiner:
  def recombine(cont: Container, count: Int, proteins: String): IArray[Int] =
    val indices = proteins.split(',').map(cont.index).distinct
    println(s"Number of proteins to consider: ${indices.length}")
    println(s"    Proteins: ${indices.map(cont.name).mkString(",")}")
    require(indices.length < 64)
    require(indices.length > count)

    val t0 = System.nanoTime()
    val matrix = IArray.tabulate(indices.length, indices.length):
      (i, j) => cont.manhattanDistance(indices(i), indices(j))
    println(f"    Time spent in matrix precalc: ${(System.nanoTime() - t0) * 1e-9}%.02f seconds")

    val recombiner = new MegaRecombiner(matrix, count)
    val t1 = System.nanoTime()
    recombiner.go(0, 0L, Double.PositiveInfinity)
    println(f"    Time spent in brute-force: ${(System.nanoTime() - t1) * 1e-9}%.02f seconds")
    val solution = indices.indices.filter(i => (recombiner.currentBestMask & (1L << i)) != 0).map(indices)
    val checkup = cont.evaluateFromScratch(solution *)
    assert(checkup == recombiner.currentMaximum)
    IArray(solution *)

class MegaRecombiner private(m: IArray[IArray[Double]], count: Int):
  private var currentMaximum = 0.0
  private var currentBestMask = 0L
  def go(index: Int, mask: Long, prefix: Double): Unit =
    assert(prefix > currentMaximum)
    val nBits = java.lang.Long.bitCount(mask)
    if nBits == count then
      currentMaximum = prefix
      currentBestMask = mask
      println(s"Updated to $currentMaximum by $currentBestMask")
    else
      assert(index < m.length)
      if m.length - index - 1 >= count - nBits then go(index + 1, mask, prefix)
      if prefix > currentMaximum then
        val fromAdded = Loops.mapMin(0, index): i =>
          if (mask & (1L << i)) != 0 then m(i)(index) else Double.PositiveInfinity
        if fromAdded > currentMaximum then
          go(index + 1, mask | (1L << index), math.min(prefix, fromAdded))
