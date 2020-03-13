package test

import java.io.File

import scala.util.{Try, Using}

object Program {

  import scala.io.StdIn.readLine

  case class Index(hashSet: Set[Int])

  sealed trait ReadFileError

  case object MissingPathArg extends ReadFileError

  case class NotDirectory(error: String) extends ReadFileError

  case class FileNotFound(t: Throwable) extends ReadFileError

  private val wordsRegex = """([A-Za-z])+""".r

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

  def index(directory: File): Index = {
    val hashSet = directory.listFiles
      .filter(_.isFile)
      .flatMap(file => {
        Using(io.Source.fromFile(file)) { source =>
          source.getLines.flatMap(wordsRegex.findAllIn).toList
        }.get
      })
      .map(word => word.toLowerCase.hashCode)
      .toSet
    Index(hashSet)
  }

  def calculateScore(searchString: String, indexedFiles: Index): Double = {
    val average = wordsRegex
      .findAllIn(searchString)
      .map(_.toLowerCase)
      .map(word => {
        indexedFiles.hashSet.contains(word.hashCode)
      })
      .map(contains => if (contains) 1 else 0)
      .map(x => (x, 1))
      .reduce((a, b) => (a._1 + b._1, a._2 + b._2))
    100 * average._1.doubleValue / average._2.doubleValue
  }

  def iterate(indexedFiles: Index): Unit = {
    var running = true;
    while (running) {
      print(s"search> ")
      val searchString = readLine()
      if (searchString.equalsIgnoreCase(":quit")) {
        running = false
      } else {
        val score = calculateScore(searchString, indexedFiles)
        println(s"score : ${score}%")
      }
    }
  }
}
