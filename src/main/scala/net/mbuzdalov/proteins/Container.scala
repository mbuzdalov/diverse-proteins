package net.mbuzdalov.proteins

class Container(seq: IArray[(String, IArray[Float])]):
  private val indexMap = seq.indices.map(i => (seq(i)._1, i)).toMap 
  
  def size: Int = seq.length
  def name(index: Int): String = seq(index)._1
  def embedding(index: Int): IArray[Float] = seq(index)._2
  def embedding(name: String): IArray[Float] = embedding(indexMap(name))
  