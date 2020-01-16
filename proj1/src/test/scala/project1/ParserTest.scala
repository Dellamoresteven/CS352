package project1

import java.io._
import org.scalatest._

class ParserTest extends FunSuite {

  def reader(src: String) = new BaseReader(src.iterator, '\u0000')
  def runner(src: String, gData: Map[Char,Int] = Map()) = new ASMRunner(src, gData)

  test("SingleDigit") {
    val gen = new SingleDigitParser(reader("4"))
    val ast = gen.parseCode

    assert(ast == Lit(4), "Invalid result")
  }

  // Function Helper for SingleAddOpParser
  def testSingleAdd(op: String, res: Exp) = {
    val gen = new SingleAddOpParser(reader(op))
    val ast = gen.parseCode

    assert(ast == res, "Invalid result")
  }

  test("SingleAddopAdd") {
    testSingleAdd("1+1", Plus(Lit(1),Lit(1)))
  }

  test("SingleAddopSub1-1") {
    testSingleAdd("1-1", Minus(Lit(1),Lit(1)))
  }

  test("SingleAddopSub1-5") {
    testSingleAdd("1-5", Minus(Lit(1),Lit(5)))
  }

  test("SingleAddopSub5-1") {
    testSingleAdd("5-1", Minus(Lit(5),Lit(1)))
  }

  // Function Helper for MultipleAddOpParser
  def testMultipleAdd(op: String, res: Exp) = {
    val gen = new MultipleAddOpParser(reader(op))
    val ast = gen.parseCode

    assert(ast == res, "Invalid result")
  }

  test("MultipleAddopAdd") {
    testMultipleAdd("1", Lit(1))
    testMultipleAdd("1+2", Plus(Lit(1), Lit(2)))
    testMultipleAdd("1+2+3", Plus(Plus(Lit(1), Lit(2)),Lit(3)))
    testMultipleAdd("1+2+3", Plus(Plus(Lit(1), Lit(2)),Lit(3)))
  }

  test("MultipleAddopSub") {
    testMultipleAdd("1-1", Minus(Lit(1), Lit(1)))
    testMultipleAdd("1-9", Minus(Lit(1), Lit(9)))
    testMultipleAdd("1-2-3", Minus(Minus(Lit(1), Lit(2)),Lit(3)))
    testMultipleAdd("1-1-1-1-1-1", Minus(Minus(Minus(Minus(Minus(Lit(1), Lit(1)),Lit(1)), Lit(1)), Lit(1)), Lit(1)))
    testMultipleAdd("0-1", Minus(Lit(0), Lit(1)))
    testMultipleAdd("1-0", Minus(Lit(1), Lit(0)))
  }


  test("MultipleAddopBoth") {
    testMultipleAdd("1-1+1", Plus(Minus(Lit(1),Lit(1)), Lit(1)))
    testMultipleAdd("1+2-3", Minus(Plus(Lit(1), Lit(2)), Lit(3)))
    testMultipleAdd("1+2+3-3-2-1", Minus(Minus(Minus(Plus(Plus(Lit(1), Lit(2)),Lit(3)) , Lit(3)), Lit(2)), Lit(1)))
    testMultipleAdd("1+2+3", Plus(Plus(Lit(1), Lit(2)),Lit(3)))
  }

  // Function Helper for ArithOpParser
  def testArith(op: String, res: Exp) = {
    val gen = new ArithOpParser(reader(op))
    val ast = gen.parseCode

    assert(ast == res, "Invalid result")
  }

  // Function Helper for ArithParOpParser
  def testArithPar(op: String, res: Exp) = {
    val gen = new ArithParOpParser(reader(op))
    val ast = gen.parseCode

    assert(ast == res, "Invalid result")
  }
}
