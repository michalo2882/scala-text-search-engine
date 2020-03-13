package test

object SimpleSearch extends App {
  Program
    .readDirectory(args)
    .fold(
      println,
      file => Program.runAppLoop(Program.buildIndex(file))
    )
}
