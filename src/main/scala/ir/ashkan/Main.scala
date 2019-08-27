package ir.ashkan

import java.io.File
import scala.util.Try
import scala.io.Source

object Main extends App {
  Program
    .readFile(args)
    .fold(
      println,
      file => Program.iterate(Program.index(file))
    )
}

object Program {

  import scala.io.StdIn.readLine

  case class Index(index: Map[String, Set[String]])

  sealed trait ReadFileError

  case object MissingPathArg extends ReadFileError

  case class NotDirectory(error: String) extends ReadFileError

  case class FileNotFound(t: Throwable) extends ReadFileError

  def readFile(args: Array[String]): Either[ReadFileError, File] = {
    for {
      path <- args.headOption.toRight(MissingPathArg)
      file <- Try(new java.io.File(path))
        .fold(
          throwable => Left(FileNotFound(throwable)),
          file =>
            if (file.isDirectory) Right(file)
            else Left(NotDirectory(s"Path [$path] is not a directory"))
        )
    } yield file
  }

  def index(file: File): Index = {
    val idx = collection.mutable.Map.empty[String, Set[String]]
    var nFiles = 0
    var nWords = 0

    file.listFiles.filter(_.isFile).toList.foreach { f =>
      nFiles += 1
      val fileName = f.getName
      print(s"Indexing '$fileName' -> ")

      val bfsrc = Source.fromFile(f)
      val words: Set[String] = bfsrc.getLines.filter(_.trim != "").flatMap(_.split("\\W+")).toSet
      for (word <- words) {
        val files = idx.getOrElse(word, Set.empty)
        idx(word.toLowerCase) = (files + fileName)
      }
      println(s"${words.size} words")
      bfsrc.close()
    }

    //    println(idx)

    println(s"Total $nFiles files and ${idx.keySet.size} words indexed.")

    Index(idx.toMap)
  }

  @scala.annotation.tailrec
  def iterate(indexedFiles: Index): Unit = {
    print(s"search>")
    val searchString = readLine()
    if (searchString != ":quit") {
      val terms = searchString.split("""\W+""").map(_.toLowerCase)
      val hits = collection.mutable.Map.empty[String, Int]
      for (term <- terms) {
        for (file <- indexedFiles.index.getOrElse(term, Set.empty)) {
          val oldScore = hits.getOrElse(file, 0)
          hits(file) = oldScore + 1
        }
      }
      println(
        hits.toMap.view
          .mapValues(_ * 100 / terms.length)
          .map[String](s => s"'${s._1}': ${s._2}%")
          .mkString("  ")
      )

      iterate(indexedFiles)
    }
  }

  def score(): Unit = println("Working.")
}