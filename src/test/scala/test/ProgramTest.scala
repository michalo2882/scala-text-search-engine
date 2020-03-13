package test

import org.scalatest.funsuite.AnyFunSuite

class ProgramTest extends AnyFunSuite {
  test("Read existing directory") {
    assert(Program.readFile(Array(".test-data")).isRight)
  }

  test("Read non-existing directory") {
    assert(Program.readFile(Array(".not-test-data")).isLeft)
  }

  test("Indexing") {
    Program.readFile(Array(".test-data")).fold(
      _ => assert(false),
      directory => {
        val index = Program.index(directory)
        assert(index.fileToFileIndexMap.contains("sample.txt"))
        assert(index.fileToFileIndexMap("sample.txt").hashSet.contains("sewis".hashCode))
        assert(index.fileToFileIndexMap("sample.txt").hashSet.contains("gentleman".hashCode))
        assert(!index.fileToFileIndexMap("sample.txt").hashSet.contains("blablablaba".hashCode))
      }
    )
  }

  test("Calculate score") {
    Program.readFile(Array(".test-data")).fold(
      _ => assert(false),
      directory => {
        val index = Program.index(directory)
        assert(Program.calculateScore("sewis", index)("sample.txt") == 100.0)
        assert(Program.calculateScore("sewis blablablaba", index)("sample.txt") == 50.0)
        assert(Program.calculateScore("   sewis    blablablaba    ", index)("sample.txt") == 50.0)
      }
    )
  }
}
