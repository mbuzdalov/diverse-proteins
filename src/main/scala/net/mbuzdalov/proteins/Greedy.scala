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

  private def findMostDistantFromOne(db: Container, index: Int): Int =
    val sample = new SampleMax(0.0)
    Loops.foreach(0, db.size): i =>
      sample.add(i, db.manhattanDistance(i, index))
    sample.sampleIndex()

  def run(db: Container, count: Int): (Solution, String) =
    val rng = ThreadLocalRandom.current()
    val seedIndex = rng.nextInt(db.size)
    val indices = new Array[Int](count)
    val minDistances = Array.fill(db.size)(Double.PositiveInfinity)
    var lastIndex = findMostDistantFromOne(db, seedIndex)
    val startName = db.name(lastIndex)
    var metric = Double.PositiveInfinity

    Loops.foreach(0, count - 1): i =>
      indices(i) = lastIndex
      val sample = new SampleMax(0.0)

      Loops.foreach(0, db.size): j =>
        val currDist = math.min(minDistances(j), db.manhattanDistance(lastIndex, j))
        minDistances(j) = currDist
        sample.add(j, currDist)

      metric = math.min(metric, sample.maximum())
      lastIndex = sample.sampleIndex()

    indices(count - 1) = lastIndex
    (Solution(IArray(indices *), metric), startName)
