package test

import java.io.File

import scala.util.{Try, Using}

object Program {

  import scala.io.StdIn.readLine

  case class FileIndex(hashSet: Set[Int])

  case class Index(fileToFileIndexMap: Map[String, FileIndex]) {

    def calculateScore(searchString: String): Map[String, Double] = {
      val words = wordsRegex
        .findAllIn(searchString)
        .map(_.toLowerCase)
        .toList
        .distinct

      fileToFileIndexMap.map { case (fileName, fileIndex) =>
        val score = words.map(word => {
          fileIndex.hashSet.contains(word.hashCode)
        })
          .map(contains => if (contains) 1 else 0)
          .map(x => (x, 1))
          .reduceOption((a, b) => (a._1 + b._1, a._2 + b._2))
          .map(average => 100 * average._1.doubleValue / average._2.doubleValue)
          .getOrElse(0.0)
        (fileName, score)
      }
    }

    def get(fileName: String): FileIndex = fileToFileIndexMap(fileName)
  }

  sealed trait ReadFileError

  case object MissingPathArg extends ReadFileError

  case class NotDirectory(error: String) extends ReadFileError

  case class FileNotFound(t: Throwable) extends ReadFileError

  private val wordsRegex = """([A-Za-z])+""".r

  def readDirectory(args: Array[String]): Either[ReadFileError, File] = {
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

  def buildIndex(directory: File): Index = {
    val fileToFileIndexMap = directory.listFiles
      .filter(_.isFile)
      .map(file => {
        val hashSet = Using(io.Source.fromFile(file)) { source =>
          source.getLines.flatMap(wordsRegex.findAllIn).toList
        }
          .get
          .map(word => word.toLowerCase.hashCode)
          .toSet
        (file.getName, FileIndex(hashSet))
      })
      .toMap
    Index(fileToFileIndexMap)
  }

  def filterScoresForDisplay(scores: List[(String, Double)]): List[(String, Double)] = {
    scores
      .filter { case (_, score) => score > 0 }
      .sortBy { case (_, score) => score }(Ordering.Double.IeeeOrdering.reverse)
      .take(10)
  }

  def runAppLoop(index: Index): Unit = {
    var running = true;
    while (running) {
      print(s"search> ")
      val searchString = readLine()
      if (searchString.equalsIgnoreCase(":quit")) {
        running = false
      } else {
        val perFileScore = filterScoresForDisplay(index.calculateScore(searchString).toList)
        if (perFileScore.isEmpty) {
          println("no matches found")
        } else {
          perFileScore.foreach {
            case (fileName, score) =>
              print(s"${fileName} : ${score}% ")
          }
          println()
        }
      }
    }
  }
}
