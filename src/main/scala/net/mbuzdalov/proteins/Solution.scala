package net.mbuzdalov.proteins

case class Solution(indices: IArray[Int], costs: Solution.NamedCost*):
  def proteinNames(cont: Container): IArray[String] = indices.map(cont.name).sorted
  def costString: String = costs.map(c => s"${c.name}: ${c.value}").mkString("[", ", ", "]")

object Solution:
  case class NamedCost(name: String, value: Double)
  