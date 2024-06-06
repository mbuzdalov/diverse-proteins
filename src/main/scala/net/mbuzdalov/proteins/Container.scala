package net.mbuzdalov.proteins

class Container(seq: IArray[(String, IArray[Float])]):
  private val indexMap = seq.indices.map(i => (seq(i)._1, i)).toMap

  def size: Int = seq.length
  def name(index: Int): String = seq(index)._1
  def index(name: String): Int = indexMap(name)
  def embedding(index: Int): IArray[Float] = seq(index)._2
  def embedding(name: String): IArray[Float] = embedding(indexMap(name))
  
  def manhattanDistance(i1: Int, i2: Int): Double = 
    Distance.manhattan(embedding(i1), embedding(i2))
    