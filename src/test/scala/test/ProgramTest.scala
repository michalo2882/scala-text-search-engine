package test

import org.scalatest.funsuite.AnyFunSuite

class ProgramTest extends AnyFunSuite {
  test("Read existing directory") {
    assert(Program.readDirectory(Array(".test-data")).isRight)
  }

  test("Read non-existing directory") {
    assert(Program.readDirectory(Array(".not-test-data")).isLeft)
  }

  test("Indexing") {
    Program.readDirectory(Array(".test-data")).fold(
      _ => assert(false),
      directory => {
        val index = Program.buildIndex(directory)
        assert(index.fileToFileIndexMap.contains("sample.txt"))
        assert(index.get("sample.txt").hashSet.contains("sewis".hashCode))
        assert(index.get("sample.txt").hashSet.contains("gentleman".hashCode))
        assert(!index.get("sample.txt").hashSet.contains("blablablaba".hashCode))
      }
    )
  }

  test("Calculate score") {
    Program.readDirectory(Array(".test-data")).fold(
      _ => assert(false),
      directory => {
        val index = Program.buildIndex(directory)
        assert(index.calculateScores("")("sample.txt") == 0.0)
        assert(index.calculateScores("sewis")("sample.txt") == 100.0)
        assert(index.calculateScores("sewis blablablaba")("sample.txt") == 50.0)
        assert(index.calculateScores("sewis sewis sewis sewis blablablaba")("sample.txt") == 50.0)
        assert(index.calculateScores("   sewis    blablablaba    ")("sample.txt") == 50.0)
      }
    )
  }

  test("filterScoresForDisplay filters out 0 scores and sorts") {
    val result = Program.filterScoresForDisplay(List(("A", 0), ("B", 1), ("C", 0), ("D", 2)))
    assert(result == List(("D", 2), ("B", 1)))
  }

  test("filterScoresForDisplay takes n-highest scores") {
    val result = Program.filterScoresForDisplay(List(("A", 0), ("B", 1), ("C", 0), ("D", 2)), take = 1)
    assert(result == List(("D", 2)))
  }
}
