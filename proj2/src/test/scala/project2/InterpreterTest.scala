package project2

import org.scalatest._

class InterpretTest extends FunSuite {
  import Language._

  def testInterpreter(ast: Exp, res: Int) = {
    val interpreter = new StackInterpreter

    assert(res == interpreter.run(ast), "Interpreter does not return the correct value")
  }

  test("arithm") {
    testInterpreter(Lit(-21), -21)
    testInterpreter(Prim("-", Lit(10), Lit(2)), 8)
    testInterpreter(Prim("+", Lit(10), Lit(15)), 25)
    testInterpreter(Prim("-",Prim("+",Prim("-",Prim("+",Prim("-",Prim("+",Lit(5),Lit(5)),Lit(5)),Lit(5)),Lit(5)),Lit(5)),Lit(5)), 5)
    testInterpreter(Unary("-", Lit(10)), -10)
    testInterpreter(Unary("+", Lit(10)), 10)
    testInterpreter(Prim("*",Lit(5),Lit(5)), 25)
    testInterpreter(Prim("*",Lit(5),Unary("-",Lit(5))), -25)
    testInterpreter(Let("x",Lit(5),Ref("x")), 5)
    testInterpreter(Let("x",Unary("-",Lit(5)),Ref("x")), -5)
    testInterpreter(VarDec("x",Lit(5),Ref("x")), 5)
    testInterpreter(VarDec("x",Unary("-",Lit(5)),Ref("x")), -5)
    testInterpreter(VarDec("x",Lit(5),VarAssign("x",Lit(3))), 3)
    testInterpreter(VarDec("x",Lit(5),If(Cond("==",Ref("x"),Lit(5)),Lit(7),Lit(6))), 7)
    testInterpreter(VarDec("x",Lit(5),If(Cond("!=",Ref("x"),Lit(5)),Lit(7),Lit(6))), 6)
    testInterpreter(While(Cond("!=",Lit(5),Lit(5)),Lit(3),Lit(5)), 5)
    testInterpreter(VarDec("x",Lit(0),While(Cond("!=",Ref("x"),Lit(5)),VarAssign("x",Prim("+",Ref("x"),Lit(1))),Ref("x"))), 5)
    testInterpreter(Let("x",Lit(5),Let("y",Lit(5),If(Cond("==",Ref("x"),Ref("y")),Lit(5),Lit(10)))), 5)
  }
}
