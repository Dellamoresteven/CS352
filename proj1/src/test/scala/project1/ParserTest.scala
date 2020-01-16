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

  // Make sure i didnt break anyhting
  test("ArithOpParseropOld") {
    testArith("1-1+1", Plus(Minus(Lit(1),Lit(1)), Lit(1)))
    testArith("1+1", Plus(Lit(1),Lit(1)))
    testArith("1-1", Minus(Lit(1),Lit(1)))
  }

  // Times
  test("ArithOpParseropTimes") {
    testArith("0*1", Times(Lit(0), Lit(1)))
    testArith("1*1", Times(Lit(1), Lit(1)))
    testArith("5*3", Times(Lit(5), Lit(3)))
    testArith("5*3*2*9", Times(Times(Times(Lit(5), Lit(3)), Lit(2)), Lit(9)))
  }

  // Div
  test("ArithOpParseropDiv") {
    testArith("0/1", Div(Lit(0), Lit(1)))
    testArith("1/1", Div(Lit(1), Lit(1)))
    testArith("5/3", Div(Lit(5), Lit(3)))
    testArith("5/3/2/9", Div(Div(Div(Lit(5), Lit(3)), Lit(2)), Lit(9)))
  }

  // Both
  test("ArithOpParseropBoth") {
    testArith("0/1*2", Times(Div(Lit(0), Lit(1)), Lit(2)))
    testArith("5*1/3", Div(Times(Lit(5), Lit(1)), Lit(3)))
    testArith("5/3*4/9", Div(Times(Div(Lit(5), Lit(3)), Lit(4)), Lit(9)))
    testArith("5/3/2/9*3*4*5*6*7/5", Div(Times(Times(Times(Times(Times(Div(Div(Div(Lit(5), Lit(3)), Lit(2)), Lit(9)), Lit(3)), Lit(4)), Lit(5)), Lit(6)), Lit(7)), Lit(5)))
    testArith("3/1*2+4", Plus(Times(Div(Lit(3), Lit(1)), Lit(2)), Lit(4)))
    testArith("4+3*3", Plus(Lit(4), Times(Lit(3), Lit(3))))
    testArith("4-3*3", Minus(Lit(4), Times(Lit(3), Lit(3))))
    testArith("4-3*3+8", Plus(Minus(Lit(4), Times(Lit(3), Lit(3))), Lit(8)))
    testArith("4-3*3+8/4", Plus(Minus(Lit(4), Times(Lit(3), Lit(3))), Div(Lit(8), Lit(4))))
  }
  // Function Helper for ArithParOpParser
  def testArithPar(op: String, res: Exp) = {
    val gen = new ArithParOpParser(reader(op))
    val ast = gen.parseCode

    assert(ast == res, "Invalid result")
  }

  test("testArithParNegative"){
    testArithPar("0/1*-2", Times(Div(Lit(0),Lit(1)),Minus(Lit(0),Lit(2))))
    testArithPar("5*-1/3", Div(Times(Lit(5), Minus(Lit(0),Lit(1))), Lit(3)))
    testArithPar("-5/3*-4/9", Div(Times(Div(Minus(Lit(0), Lit(5)), Lit(3)), Minus( Lit(0), Lit(4))), Lit(9)))
    testArithPar("5/3/2/9*3*-4*-5*6*7/5", Div(Times(Times(Times(Times(Times(Div(Div(Div(Lit(5), Lit(3)), Lit(2)), Lit(9)), Lit(3)), Minus(Lit(0), Lit(4))), Minus(Lit(0), Lit(5))), Lit(6)), Lit(7)), Lit(5)))
    testArithPar("3/1*2+4", Plus(Times(Div(Lit(3), Lit(1)), Lit(2)), Lit(4)))
    testArithPar("-4+-3*-3", Plus(Minus(Lit(0), Lit(4)), Times(Minus(Lit(0), Lit(3)), Minus(Lit(0), Lit(3)))))
    testArithPar("4-3*3", Minus(Lit(4), Times(Lit(3), Lit(3))))
    testArithPar("4-3*3+8", Plus(Minus(Lit(4), Times(Lit(3), Lit(3))), Lit(8)))
    testArithPar("4-3*3+8/4", Plus(Minus(Lit(4), Times(Lit(3), Lit(3))), Div(Lit(8), Lit(4))))
  }
}
