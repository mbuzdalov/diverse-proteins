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
  private def readEmbeddingsForever(source: ObjectInputStream,
                                    target: mutable.Growable[(String, IArray[Float])],
                                    filter: String => Boolean): Nothing =
    val key = source.readUTF()
    val value = source.readObject().asInstanceOf[IArray[Float]]
    if filter(key) then target += (key -> value)
    readEmbeddingsForever(source, target, filter)

  private def readEmbeddings(source: String, filterFile: Option[String]): Container =
    val startTime = System.nanoTime()
    val filterFun = filterFile match
      case None => (name: String) => true
      case Some(f) => Using.resource(new BufferedReader(new FileReader(f))): br =>
        val setB = Set.newBuilder[String]
        var line = br.readLine()
        while line != null do
          setB += line
          line = br.readLine()
        val set = setB.result()
        set.contains

    Using.resource(new ObjectInputStream(new GZIPInputStream(new FileInputStream(source)))): in =>
      val builder = IArray.newBuilder[(String, IArray[Float])]
      try
        readEmbeddingsForever(in, builder, filterFun)
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

  private def readEmbeddingsWrapper(args: Array[String]): Container =
    val mainFile = args(1)
    val indexOfSubset = args.indexOf("--subset")
    val subsetFile = if indexOfSubset < 0 then None else Some(args(indexOfSubset + 1))
    readEmbeddings(mainFile, subsetFile)
  
  def main(args: Array[String]): Unit =
    args(0) match
      case "import" => Importing.importEmbeddings(args(1), args(2))
      case "stats" =>
        val data = readEmbeddingsWrapper(args)
        println(s"${data.size} proteins")
      case "greedy" =>
        val count = args(2).toInt
        val data = readEmbeddingsWrapper(args)
        runParallel(100):
          val t0 = System.nanoTime()
          val (solution, first) = Greedy.run(data, count)
          Main.synchronized:
            printTimeSpentSince(t0)
            println(data.explainSolution(solution))
      case "measure" =>
        val sets = args.drop(2)
        val data = readEmbeddingsWrapper(args)
        evaluate(data, sets)
      case "local-rnd" =>
        val useMin = args(3) == "min"
        val count = args(2).toInt
        val data = readEmbeddingsWrapper(args)
        runParallel(100):
          val t0 = System.nanoTime()
          val solution = LocalSearch.optimize(data, count, useMin)
          Main.synchronized:
            printTimeSpentSince(t0)
            println(data.explainSolution(solution))
      case "local-greedy" =>
        val useMin = args(3) == "min"
        val count = args(2).toInt
        val data = readEmbeddingsWrapper(args)
        runParallel(100):
          val t0 = System.nanoTime()
          val (initial, _) = Greedy.run(data, count)
          val solution = LocalSearch.optimize(data, initial, useMin)
          Main.synchronized:
            printTimeSpentSince(t0)
            println(data.explainSolution(solution))
      case "recombine" =>
        val count = args(2).toInt
        val proteins = args.drop(3).mkString(",").replace(",,", ",")
        val data = readEmbeddingsWrapper(args)
        val solution = MegaRecombiner.recombine(data, count, proteins)
        println(data.explainSolution(solution))
      case "heatmap" =>
        val data = readEmbeddingsWrapper(args)
        for a <- args.drop(2) do
          val eq = a.indexOf('=')
          val filename = a.take(eq)
          val proteins = a.drop(eq + 1).split(',').toIndexedSeq
          Heatmaps.draw(data, proteins, filename)

  end main
