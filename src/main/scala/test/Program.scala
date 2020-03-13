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
    val wordsRegex = """([A-Za-z])+""".r
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

  def iterate(indexedFiles: Index): Unit = {
    print(s"search> ")
    val searchString = readLine()
    // TODO: Make it print the ranking of each file and its corresponding score
    iterate(indexedFiles)
  }
}
