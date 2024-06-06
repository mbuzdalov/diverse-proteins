package net.mbuzdalov.proteins

import java.util.concurrent.ThreadLocalRandom

import scala.collection.immutable.TreeSet

object Greedy:
  private def findMostDistant(db: Container, indices: Int*): (Int, Double) =
    val listMostDistant = Array.newBuilder[Int]
    var minDistance = 0.0

    Loops.foreach(0, db.size): i =>
      val curr = db.embedding(i)
      val locallyMin = Loops.mapMin(0, indices.length)(j => Distance.manhattan(curr, db.embedding(indices(j))))
      if locallyMin > minDistance then
        minDistance = locallyMin
        listMostDistant.clear()
      if locallyMin == minDistance then
        listMostDistant += i

    val toSample = listMostDistant.result()
    (toSample(ThreadLocalRandom.current().nextInt(toSample.length)), minDistance)

  def run(db: Container, count: Int): (Set[String], Double, String) =
    val rng = ThreadLocalRandom.current()
    val indices = new Array[Int](count)
    val seedIndex = rng.nextInt(db.size)
    val (i0, m0) = findMostDistant(db, seedIndex)
    indices(0) = i0

    val metric = Loops.mapMin(1, count): i =>
      val (ii, mi) = findMostDistant(db, indices.take(i) *)
      indices(i) = ii
      mi

    (TreeSet(indices.map(db.name) *), metric, db.name(i0))
