package project2

import org.scalatest._

class SemanticAnalyzerTest extends FunSuite {
  import Language._

  def testSemanticAnalyzer(ast: Exp, nWarning: Int, nError: Int) = {
    val fakeParser = new Parser(null) {
      override def error(msg: String, pos: Position) = {}
      override def warn(msg: String, pos: Position) = {}
    }

    val analyzer = new SemanticAnalyzer(fakeParser)

    val (w, e) = analyzer.run(ast)
    assert(w == nWarning, "Incorrect number of Warnings")
    assert(e == nError, "Incorrect number of Errors")
  }

  test("29") {
    testSemanticAnalyzer(Lit(1), 0, 0)
    testSemanticAnalyzer(Prim("+", Lit(1), Lit(2)), 0, 0)
  }
  test("30"){
    testSemanticAnalyzer(Unary("*",Lit(1)),0,1)
  }
  test("31"){
    testSemanticAnalyzer(Prim("%",Lit(1),Lit(5)),0,1)
  }
  test("32"){
    testSemanticAnalyzer(VarDec("x",Lit(1),Let("x",Lit(5),Lit(4))), 1 ,0)
  }
  test("3"){
    testSemanticAnalyzer(Prim("%",Lit(1),Lit(5)),0,1)
  }
  test("steve110001"){
    testSemanticAnalyzer(Let("x",Lit(5),VarAssign("x",Lit(6))),0,1)
  }
  test("steve110002"){
    testSemanticAnalyzer(Let("x",Lit(5),Let("x",Lit(6),Ref("x"))), 1, 0)
  }
  test("steve110003"){
    testSemanticAnalyzer(Let("x",Lit(5),Let("x",Lit(6),VarAssign("x",Lit(5)))), 1, 1)
  }
  test("steve110004"){
    testSemanticAnalyzer(Prim("+",Unary("/",Lit(5)),Lit(5)), 0, 1)
  }
  test("steve110005"){
    testSemanticAnalyzer(Prim("++",Lit(5),Lit(5)), 0, 1)
  }
  test("steve110006"){
    testSemanticAnalyzer(If(Cond("====",Lit(3),Lit(3)),Lit(5),Lit(4)), 0, 1)
  }
  test("steve110007"){
    testSemanticAnalyzer(VarDec("x",Lit(5),VarDec("x",Lit(6),Ref("x"))), 1, 0)
  }
  test("steve110008"){
    testSemanticAnalyzer(If(Cond("==",Ref("x"),Lit(5)),Lit(5),Lit(3)), 0, 1)
  }
  /**test("3134"){
    testSemanticAnalyzer(Prim("%",Lit(1),Lit(5)),0,1)
  }*/
}
