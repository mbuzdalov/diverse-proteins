package net.mbuzdalov.proteins

import java.io.*
import java.util.{Locale, StringTokenizer}
import java.util.concurrent.{Callable, ScheduledThreadPoolExecutor}
import java.util.zip.GZIPInputStream

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Using

object Main:
  @tailrec
  private def readEmbeddingsForever(source: ObjectInputStream, target: mutable.Growable[(String, IArray[Float])]): Nothing =
    target += (source.readUTF() -> source.readObject().asInstanceOf[IArray[Float]])
    readEmbeddingsForever(source, target)

  private def readEmbeddings(source: String): Container =
    val startTime = System.nanoTime()
    Using.resource(new ObjectInputStream(new GZIPInputStream(new FileInputStream(source)))): in =>
      val builder = IArray.newBuilder[(String, IArray[Float])]
      try
        readEmbeddingsForever(in, builder)
      catch
        case e: EOFException =>
          println("# Embeddings loaded in %.01f seconds".formatLocal(Locale.US, (System.nanoTime() - startTime) * 1e-9))
          new Container(builder.result())

  private def evaluate(db: Container, sets: Array[String]): Unit =
    for set <- sets do
      val tok = new StringTokenizer(set, ",:")
      val name = tok.nextToken()
      val names = Array.fill(tok.countTokens())(tok.nextToken())
      val indices = names.map(db.index)
      val result = db.evaluateFromScratch(indices *)
      println(s"$result <= $name (${names.sorted.mkString(", ")})")

  private def runParallel(times: Int)(fun: => Unit): Unit =
    import scala.jdk.CollectionConverters.given
    val pool = new ScheduledThreadPoolExecutor(Runtime.getRuntime.availableProcessors())
    val collection = (0 until times).map[Callable[Unit]](_ => () => fun).asJava
    pool.invokeAll(collection).asScala.foreach(_.get())
    pool.shutdown()

  private def printTimeSpentSince(timeNanos: Long): Unit =
    println("# Time spent: %.01f seconds".formatLocal(Locale.US, (System.nanoTime() - timeNanos) * 1e-9))

  def main(args: Array[String]): Unit =
    args(0) match
      case "import" => Importing.importEmbeddings(args(1), args(2))
      case "stats" =>
        val data = readEmbeddings(args(1))
        println(s"${data.size} proteins")
      case "greedy" =>
        val count = args(2).toInt
        val data = readEmbeddings(args(1))
        runParallel(100):
          val t0 = System.nanoTime()
          val (solution, first) = Greedy.run(data, count)
          Main.synchronized:
            printTimeSpentSince(t0)
            println(data.explainSolution(solution))
      case "measure" =>
        val sets = args.drop(2)
        val data = readEmbeddings(args(1))
        evaluate(data, sets)
      case "local-min-min" =>
        val count = args(2).toInt
        val data = readEmbeddings(args(1))
        runParallel(100):
          val t0 = System.nanoTime()
          val solution = LocalSearchMinMin.optimize(data, count)
          Main.synchronized:
            printTimeSpentSince(t0)
            println(data.explainSolution(solution))
      case "local-min-sum" =>
        val count = args(2).toInt
        val data = readEmbeddings(args(1))
        runParallel(100):
          val t0 = System.nanoTime()
          val solution = LocalSearchMinSum.optimize(data, count)
          Main.synchronized:
            printTimeSpentSince(t0)
            println(data.explainSolution(solution))
      case "recombine" =>
        val count = args(2).toInt
        val proteins = args.drop(3).mkString(",").replace(",,", ",")
        val data = readEmbeddings(args(1))
        val solution = MegaRecombiner.recombine(data, count, proteins)
        println(data.explainSolution(solution))
  end main
