package ir.ashkan

import java.io.File

import scala.util.Try
import scala.io.{Codec, Source}

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
    var nErrors = 0
    val codec = Codec("UTF-8")

    file.listFiles.filter(_.isFile).toList.foreach { f =>
      nFiles += 1
      val fileName = f.getName
      print(s"Indexing '$fileName' -> ")

      val bfsrc = Source.fromFile(f)(codec)
      try {
        val lines = bfsrc.getLines.filter(_.trim != "")
        val words = lines.flatMap(_.split("\\W+")).toSet
        for (word <- words) {
          val files = idx.getOrElse(word, Set.empty)
          idx(word.toLowerCase) = (files + fileName)
        }
        println(s"${words.size} words")
      } catch {
        case e =>
          nErrors += 1
          println(s"ERR ${e.getMessage}")
      } finally {
        bfsrc.close()
      }
    }

    println(s"Total $nFiles files and ${idx.keySet.size} words indexed, $nErrors error(s) encountered.")

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
        if (hits.nonEmpty)
          hits
            .toMap
            .view.mapValues(_ * 100 / terms.length)
            .map[String](s => s"'${s._1}': ${s._2}%")
            .mkString("  ")
        else
          "No matches found"
      )

      iterate(indexedFiles)
    }
  }

  def score(): Unit = println("Working.")
}