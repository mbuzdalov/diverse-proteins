package net.mbuzdalov.proteins

case class Solution(indices: IArray[Int], cost: Double):
  def proteinNames(cont: Container): IArray[String] = indices.map(cont.name).sorted
