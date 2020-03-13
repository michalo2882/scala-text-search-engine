package test

object SimpleSearch extends App {
  Program
    .readFile(args)
    .fold(
      println,
      file => Program.iterate(Program.buildIndex(file))
    )
}
