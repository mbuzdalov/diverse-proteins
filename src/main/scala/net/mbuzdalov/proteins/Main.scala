package net.mbuzdalov.proteins

import java.io.*
import java.util.StringTokenizer
import java.util.zip.GZIPInputStream

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Using

object Main:
  @tailrec
  private def readEmbeddingsForever(source: ObjectInputStream, target: mutable.Growable[(String, IArray[Float])]): Nothing =
    target += (source.readUTF() -> source.readObject().asInstanceOf[IArray[Float]])
    readEmbeddingsForever(source, target)

  private def readEmbeddings(source: String): Map[String, IArray[Float]] =
    Using.resource(new ObjectInputStream(new GZIPInputStream(new FileInputStream(source)))): in =>
      val builder = Map.newBuilder[String, IArray[Float]]
      try
        readEmbeddingsForever(in, builder)
      catch
        case e: EOFException => builder.result()

  private def evaluate(db: Map[String, IArray[Float]], sets: Array[String]): Unit =
    for set <- sets do
      val tok = new StringTokenizer(set, ",:")
      val name = tok.nextToken()
      val data = Array.fill(tok.countTokens())(tok.nextToken())
      var result = Double.PositiveInfinity
      for i <- data.indices; j <- i + 1 until data.length do
        result = math.min(result, Distance.manhattan(db(data(i)), db(data(j))))
      println(s"$result <= $name (${data.sorted.mkString(", ")})")

  def main(args: Array[String]): Unit =
    args(0) match
      case "import" => Importing.importEmbeddings(args(1), args(2))
      case "stats" =>
        val data = readEmbeddings(args(1))
        println(s"${data.size} proteins")
        println(s"${data.map(_._2.length.toLong).sum * 4} total size in bytes")
      case "greedy" =>
        val count = args(2).toInt
        val data = readEmbeddings(args(1))
        for run <- 0 until 4 do
          val (set, metric) = Greedy.run(data, count)
          println(s"Found maxmin value: $metric")
          println(s"Proteins: ${set.mkString(", ")}")
      case "measure" =>
        val sets = args.drop(2)
        val data = readEmbeddings(args(1))
        evaluate(data, sets)
  end main
