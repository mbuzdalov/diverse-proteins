package net.mbuzdalov.proteins

import java.util.concurrent.ThreadLocalRandom

import scala.collection.immutable.TreeSet

object Greedy:
  private def findMostDistant(db: Array[(String, IArray[Float])], indices: Int*): (Int, Double) =
    val listMostDistant = Array.newBuilder[Int]
    var minDistance = 0.0
    var i = 0
    while i < db.length do
      val curr = db(i)._2
      var locallyMin = Double.MaxValue
      var j = 0
      while j < indices.length do
        locallyMin = math.min(locallyMin, Distance.manhattan(curr, db(indices(j))._2))
        j += 1
      if locallyMin > minDistance then
        minDistance = locallyMin
        listMostDistant.clear()
      if locallyMin == minDistance then
        listMostDistant += i
      i += 1

    val toSample = listMostDistant.result()
    (toSample(ThreadLocalRandom.current().nextInt(toSample.length)), minDistance)

  def run(db: Map[String, IArray[Float]], count: Int): (Set[String], Double) =
    println("Starting greedy")
    val ar = db.toArray
    val rng = ThreadLocalRandom.current()
    val indices = new Array[Int](count)
    val seedIndex = rng.nextInt(ar.length)
    println(s"  seed index: $seedIndex")
    val (i0, m0) = findMostDistant(ar, seedIndex)
    println(s"  #1 found (${ar(i0)._1})")
    indices(0) = i0
    var metric = m0
    var i = 1
    while i < count do
      val (ii, mi) = findMostDistant(ar, indices.take(i) *)
      println(s"  #${i + 1} found (${ar(ii)._1})")
      indices(i) = ii
      metric = math.min(metric, mi)
      i += 1
    (TreeSet(indices.map(i => ar(i)._1) *), metric)
