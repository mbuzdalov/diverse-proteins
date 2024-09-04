package net.mbuzdalov.proteins

import java.util.concurrent.ThreadLocalRandom

object Greedy:
  private class SampleMax(baseValue: Double):
    private val list = Array.newBuilder[Int]
    private var value: Double = baseValue

    def add(index: Int, value: Double): Unit =
      if value > this.value then
        list.clear()
        this.value = value
      if value == this.value then
        list += index

    def sampleIndex(): Int =
      val list = this.list.result()
      list(ThreadLocalRandom.current().nextInt(list.length))

    def maximum(): Double = value

    def reset(): Unit =
      value = baseValue
      list.clear()

  def run(db: Container, count: Int): (Solution, String) =
    val rng = ThreadLocalRandom.current()
    val sample = new SampleMax(0.0)

    // Sample a random pre-starting element
    val seedIndex = rng.nextInt(db.size)
    Loops.foreach(0, db.size): i =>
      sample.add(i, db.manhattanDistance(i, seedIndex))
    var lastIndex = sample.sampleIndex()
    val startName = db.name(lastIndex)

    // lastIndex will always be the index of the last protein added
    // Initially it is the most distant protein from the pre-starting element

    // We track for each element the minimum distance to the already added elements,
    // where lastIndex is added in the beginning of the loop, so initially all are infinite.
    val minDistances = Array.fill(db.size)(Double.PositiveInfinity)
    val indices = new Array[Int](count)
    var metric = Double.PositiveInfinity

    Loops.foreach(0, count - 1): i =>
      indices(i) = lastIndex
      // Update all the minima and choose the maximum of them.
      // The already added elements are effectively zeroed out.
      sample.reset()
      Loops.foreach(0, db.size): j =>
        val currDist = math.min(minDistances(j), db.manhattanDistance(lastIndex, j))
        minDistances(j) = currDist
        sample.add(j, currDist)

      metric = math.min(metric, sample.maximum())
      lastIndex = sample.sampleIndex()

    // The very last element has to be added explicitly.
    // We save some time by not comparing it with all other proteins.
    indices(count - 1) = lastIndex
    (Solution(IArray(indices *), metric), startName)
