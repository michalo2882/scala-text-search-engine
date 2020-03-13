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
        assert(index.calculateScore("")("sample.txt") == 0.0)
        assert(index.calculateScore("sewis")("sample.txt") == 100.0)
        assert(index.calculateScore("sewis blablablaba")("sample.txt") == 50.0)
        assert(index.calculateScore("sewis sewis sewis sewis blablablaba")("sample.txt") == 50.0)
        assert(index.calculateScore("   sewis    blablablaba    ")("sample.txt") == 50.0)
      }
    )
  }
}
