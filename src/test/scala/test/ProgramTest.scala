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
        assert(index.hashSet.contains("sewis".hashCode))
        assert(index.hashSet.contains("gentleman".hashCode))
        assert(!index.hashSet.contains("blablablaba".hashCode))
      }
    )
  }
}
