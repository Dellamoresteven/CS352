package project1

import java.io._
import org.scalatest._

// Define the stream method
trait TestOutput {
  val out = new ByteArrayOutputStream
  val pOut = new PrintWriter(out, true)
  def stream = pOut
  def emitCode(ast: Exp): Unit

  def code(ast: Exp) = {
    emitCode(ast)
    out.toString.stripLineEnd
  }
}

class StackGeneratorTest extends FunSuite {

  def runner(src: String, gData: Map[Char,Int] = Map()) = new ASMRunner(src, gData)

  // Function Helper for StackASMGenerator
  def testStackASMGenerator(ast: Exp, res: Int) = {
    val gen = new StackASMGenerator with TestOutput

    val code = gen.code(ast)
    val asm = runner(code)

    assert(asm.assemble == 0, "Code generated couldn't be assembled")
    assert(asm.run == res, "Invalid result")
  }

  test("SingleDigit") {
    testStackASMGenerator(Lit(2), 2)
  }

  test("Plus") {
    testStackASMGenerator(Plus(Lit(3),Lit(2)), 5)
    testStackASMGenerator(Plus(Lit(3),Minus(Lit(0), Lit(2))), 1)
    testStackASMGenerator(Plus(Lit(3),Minus(Lit(0), Lit(5))), -2)
    testStackASMGenerator(Plus(Lit(3),Minus(Lit(0), Lit(3))), 0)
  }

  test("Minus") {
    testStackASMGenerator(Minus(Lit(3),Lit(2)), 1)
    testStackASMGenerator(Minus(Lit(3),Lit(3)), 0)
    testStackASMGenerator(Minus(Lit(3),Lit(9)), -6)
  }

  test("Mult") {
    testStackASMGenerator(Times(Lit(3),Lit(2)), 6)
    testStackASMGenerator(Div(Times(Lit(3),Lit(2)), Lit(3)), 2)
    testStackASMGenerator(Div(Times(Lit(3), Minus(Lit(0), Lit(2))), Lit(3)), -2)
    testStackASMGenerator(Div(Times(Lit(3), Minus(Lit(0), Lit(2))), Minus(Lit(0), Lit(3))), 2)
  }

  test("Div") {
    testStackASMGenerator(Div(Lit(3),Lit(2)), 1)
    testStackASMGenerator(Div(Lit(3),Lit(1)), 3)
    testStackASMGenerator(Div(Lit(0),Lit(5)), 0)
    testStackASMGenerator(Div(Lit(9),Lit(3)), 3)
    testStackASMGenerator(Div(Lit(9),Lit(4)), 2)
    testStackASMGenerator(Div(Lit(9),Minus(Lit(0), Lit(4))), -2)
    testStackASMGenerator(Div(Minus(Lit(0),Lit(9)),Minus(Lit(0), Lit(4))), 2)
    testStackASMGenerator(Div(Minus(Lit(0), Lit(9)),Lit(4)), -2)
  }
}

class RegGeneratorTest extends FunSuite {

  def runner(src: String, gData: Map[Char,Int] = Map()) = new ASMRunner(src, gData)

  // Function Helper for StackASMGenerator
  def testRegASMGenerator(ast: Exp, res: Int) = {
    val gen = new RegASMGenerator with TestOutput

    val code = gen.code(ast)
    val asm = runner(code)

    assert(asm.assemble == 0, "Code generated couldn't be assembled")
    assert(asm.run == res, "Invalid result")
  }

  test("SingleDigit") {
    testRegASMGenerator(Lit(2), 2)
  }

  test("Plus") {
    testRegASMGenerator(Plus(Lit(3),Lit(2)), 5)
    testRegASMGenerator(Plus(Lit(3),Minus(Lit(0), Lit(2))), 1)
    testRegASMGenerator(Plus(Lit(3),Minus(Lit(0), Lit(5))), -2)
    testRegASMGenerator(Plus(Lit(3),Minus(Lit(0), Lit(3))), 0)
  }

  test("Minus") {
    testRegASMGenerator(Minus(Lit(3),Lit(2)), 1)
    testRegASMGenerator(Minus(Lit(3),Lit(3)), 0)
    testRegASMGenerator(Minus(Lit(3),Lit(9)), -6)
  }

  test("Mult") {
    testRegASMGenerator(Times(Lit(3),Lit(2)), 6)
    testRegASMGenerator(Div(Times(Lit(3),Lit(2)), Lit(3)), 2)
    testRegASMGenerator(Div(Times(Lit(3), Minus(Lit(0), Lit(2))), Lit(3)), -2)
    testRegASMGenerator(Div(Times(Lit(3), Minus(Lit(0), Lit(2))), Minus(Lit(0), Lit(3))), 2)
  }

  test("Div") {
    testRegASMGenerator(Div(Lit(3),Lit(2)), 1)
    testRegASMGenerator(Div(Lit(3),Lit(1)), 3)
    testRegASMGenerator(Div(Lit(0),Lit(5)), 0)
    testRegASMGenerator(Div(Lit(9),Lit(3)), 3)
    testRegASMGenerator(Div(Lit(9),Lit(4)), 2)
    testRegASMGenerator(Div(Lit(9),Minus(Lit(0), Lit(4))), -2)
    testRegASMGenerator(Div(Minus(Lit(0),Lit(9)),Minus(Lit(0), Lit(4))), 2)
    testRegASMGenerator(Div(Minus(Lit(0), Lit(9)),Lit(4)), -2)
  }

  // TODO more tests
}
