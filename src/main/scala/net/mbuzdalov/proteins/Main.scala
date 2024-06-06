package net.mbuzdalov.proteins

import java.io.*
import java.util.StringTokenizer
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
    Using.resource(new ObjectInputStream(new GZIPInputStream(new FileInputStream(source)))): in =>
      val builder = IArray.newBuilder[(String, IArray[Float])]
      try
        readEmbeddingsForever(in, builder)
      catch
        case e: EOFException => new Container(builder.result())

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

  def main(args: Array[String]): Unit =
    args(0) match
      case "import" => Importing.importEmbeddings(args(1), args(2))
      case "stats" =>
        val data = readEmbeddings(args(1))
        println(s"${data.size} proteins")
        println(s"${Loops.mapSum(0, data.size)(i => data.embedding(i).length.toLong) * 4} total size in bytes")
      case "greedy" =>
        val count = args(2).toInt
        val data = readEmbeddings(args(1))
        runParallel(100):
          val t0 = System.nanoTime()
          val (solution, first) = Greedy.run(data, count)
          Main.synchronized:
            println(f"# Time spent: ${(System.nanoTime() - t0) * 1e-9}%01f seconds")
            println(s"Fitness ${solution.cost} when starting at $first, proteins ${solution.proteinNames(data).mkString(", ")}")
      case "measure" =>
        val sets = args.drop(2)
        val data = readEmbeddings(args(1))
        evaluate(data, sets)
      case "local" =>
        val count = args(2).toInt
        val data = readEmbeddings(args(1))
        runParallel(100):
          val t0 = System.nanoTime()
          val solution = LocalSearch.optimize(data, count)
          Main.synchronized:
            println(f"# Time spent: ${(System.nanoTime() - t0) * 1e-9}%01f seconds")
            println(s"Fitness ${solution.cost}, proteins ${solution.proteinNames(data).mkString(", ")}")

  end main
