package project2

import org.scalatest._
import java.io.{ByteArrayOutputStream, PrintWriter}

// Define the stream method
trait TestOutput {
  import Language._

  val out = new ByteArrayOutputStream
  val pOut = new PrintWriter(out, true)
  def stream = pOut
  def emitCode(ast: Exp): Unit

  def code(ast: Exp) = {
    emitCode(ast)
    out.toString.stripLineEnd
  }
}

class CompilerTest extends FunSuite {
  import Language._

  def runner(src: String, gData: Map[Char,Int] = Map()) = new ASMRunner(src, gData)

  def testCompiler(ast: Exp, res: Int) = {
    val interpreter = new X86Compiler with TestOutput

    val code = interpreter.code(ast)
    val asm = runner(code)

    assert(asm.assemble == 0, "Code generated couldn't be assembled")
    assert(asm.run == res, "Invalid result")
  }

  test("arithm") {
    testCompiler(Lit(-21), -21)
    testCompiler(Prim("-", Lit(10), Lit(2)), 8)
    testCompiler(Prim("+", Lit(10), Lit(15)), 25)
    testCompiler(Prim("-",Prim("+",Prim("-",Prim("+",Prim("-",Prim("+",Lit(5),Lit(5)),Lit(5)),Lit(5)),Lit(5)),Lit(5)),Lit(5)), 5)
    testCompiler(Unary("-", Lit(10)), -10)
    testCompiler(Unary("+", Lit(10)), 10)
    testCompiler(Prim("*",Lit(5),Lit(5)), 25)
    testCompiler(Prim("*",Lit(5),Unary("-",Lit(5))), -25)
    testCompiler(Let("x",Lit(5),Ref("x")), 5)
    testCompiler(Let("x",Unary("-",Lit(5)),Ref("x")), -5)
    testCompiler(VarDec("x",Lit(5),Ref("x")), 5)
    testCompiler(VarDec("x",Unary("-",Lit(5)),Ref("x")), -5)
    testCompiler(VarDec("x",Lit(5),VarAssign("x",Lit(3))), 3)
    testCompiler(VarDec("x",Lit(5),If(Cond("==",Ref("x"),Lit(5)),Lit(7),Lit(6))), 7)
    testCompiler(VarDec("x",Lit(5),If(Cond("!=",Ref("x"),Lit(5)),Lit(7),Lit(6))), 6)
    testCompiler(While(Cond("!=",Lit(5),Lit(5)),Lit(3),Lit(5)), 5)
    testCompiler(VarDec("x",Lit(0),While(Cond("!=",Ref("x"),Lit(5)),VarAssign("x",Prim("+",Ref("x"),Lit(1))),Ref("x"))), 5)
    testCompiler(Let("x",Lit(5),Let("y",Lit(5),If(Cond("==",Ref("x"),Ref("y")),Lit(5),Lit(10)))), 5)
  }

}
