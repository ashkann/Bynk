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
    val codec = Codec("UTF-8")

    val (files, words, errs) = file.listFiles.filter(_.isFile).toList.foldLeft((0, 0, 0)) { (n, f) =>
      val fileName = f.getName
      print(s"Indexing '$fileName' -> ")

      val src = Source.fromFile(f)(codec)
      val (df, dw, de, result) = Try {
        src
          .getLines
          .filter(_.trim != "")
          .flatMap(_.split("\\W+"))
          .toSet
          .foldLeft(0) { (nw, word) =>
            idx.updateWith(word.toLowerCase) {
              case Some(files) => Option(files + fileName)
              case None => Option(Set(fileName))
            }
            nw + 1
          }
      }.fold(e => (0, 0, 1, s"ERR ${e.getMessage}"), nw => (1, nw, 0, s"$nw words"))

      println(result)
      src.close()

      ((f: Int, w: Int, e: Int) => (df + f, dw + w, de + e)).tupled(n)
    }

    println(s"Total $files files and $words words indexed, $errs error(s) encountered.")

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

  sealed trait ReadFileError

  case class Index(index: Map[String, Set[String]])

  case class NotDirectory(error: String) extends ReadFileError

  case class FileNotFound(t: Throwable) extends ReadFileError

  case object MissingPathArg extends ReadFileError

}
