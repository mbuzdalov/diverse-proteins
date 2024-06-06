package net.mbuzdalov.proteins

import java.io.{BufferedOutputStream, FileOutputStream, ObjectOutputStream}
import java.nio.file.Path

import scala.util.Using
import scala.jdk.CollectionConverters.given

import io.jhdf.HdfFile
import io.jhdf.api.Dataset

object Importing:
  def importEmbeddings(source: String, destination: String): Unit =
    Using.resource(new HdfFile(Path.of(source))): hdf =>
      Using.resource(new ObjectOutputStream(new BufferedOutputStream(new FileOutputStream(destination)))): out =>
        for node <- hdf.iterator().asScala do
          val ds = node.asInstanceOf[Dataset]
          out.writeUTF(node.getName)
          out.writeObject(ds.getData)
