package test

import org.scalatest.funsuite.AnyFunSuite

class ProgramTest extends AnyFunSuite {
  test("Read existing directory") {
    assert(Program.readFile(Array(".test-data")).isRight)
  }

  test("Read unexisting directory") {
    assert(Program.readFile(Array(".not-test-data")).isLeft)
  }
}
