package miniscala.test

import miniscala.test.infrastructure.CPSHighTest
import org.junit.Test

class CMScalaToCPS_Whitebox_Cond extends CPSHighTest {
  @Test def testCondNestedTrueTrue =
    testCPSHighTreeEquality("if (if (3 == 4) true else true) 1 else 2", """
      defc v$1(v$2) = { vall v$3 = 0; halt(v$3) };
      |defc v$4() = { vall v$5 = 1; v$1(v$5) };
      |defc v$6() = { vall v$7 = 2; v$1(v$7) };
      |vall v$8 = 3;
      |vall v$9 = 4;
      |if (v$8 == v$9) v$4 else v$4
      """.stripMargin)

  @Test def testCondNestedTruefalse =
    testCPSHighTreeEquality("if (if (3 == 4) true else false) 1 else 2", """
      defc v$1(v$2) = { vall v$3 = 0; halt(v$3) };
      defc v$4() = { vall v$5 = 1; v$1(v$5) };
      defc v$6() = { vall v$7 = 2; v$1(v$7) };
      vall v$8 = 3;
      vall v$9 = 4;
      if (v$8 == v$9) v$4 else v$6
      """.stripMargin)

  @Test def testCondNestedfalsefalse =
    testCPSHighTreeEquality("if (if (3 == 4) false else false) 1 else 2", """
      defc v$1(v$2) = { vall v$3 = 0; halt(v$3) };
      defc v$4() = { vall v$5 = 1; v$1(v$5) };
      defc v$6() = { vall v$7 = 2; v$1(v$7) };
      vall v$8 = 3;
      vall v$9 = 4;
      if (v$8 == v$9) v$6 else v$6
      """.stripMargin)

  @Test def testCondNestedfalsetrue =
    testCPSHighTreeEquality("if (if (3 == 4) false else true) 1 else 2", """
      defc v$1(v$2) = { vall v$3 = 0; halt(v$3) };
      defc v$4() = { vall v$5 = 1; v$1(v$5) };
      defc v$6() = { vall v$7 = 2; v$1(v$7) };
      vall v$8 = 3;
      vall v$9 = 4;
      if (v$8 == v$9) v$6 else v$4
      """.stripMargin)

  @Test def testCondNestedfalsetrueFunction =
    testCPSHighTreeEquality("def g(x: Int):Boolean = {true}; if (if (g(3) == true) false else true) 1 else 2", """
        deff v$1(v$2, v$3) = { vall v$4 = true; v$2(v$4) };
        defc v$5(v$6) = { vall v$7 = 0; halt(v$7) };
        defc v$8() = { vall v$9 = 1; v$5(v$9) };
        defc v$10() = { vall v$11 = 2; v$5(v$11) };
        vall v$12 = 3;
        defc v$13(v$14) = { vall v$15 = true; if (v$14 == v$15) v$10 else v$8 };
        v$1(v$13, v$12)
      """.stripMargin)

  @Test def testCondNestedfalsetrueFunction2 =
    testCPSHighTreeEquality("def g(x: Int):Boolean = {true}; if (if (g(3)) false else true) 1 else 2", """
      deff v$1(v$2, v$3) = { vall v$4 = true; v$2(v$4) };
      defc v$5(v$6) = { vall v$7 = 0; halt(v$7) };
      defc v$8() = { vall v$9 = 1; v$5(v$9) };
      defc v$10() = { vall v$11 = 2; v$5(v$11) };
      vall v$12 = 3;
      defc v$13(v$14) = { vall v$15 = false; if (v$14 != v$15) v$10 else v$8 };
      v$1(v$13, v$12)
      """.stripMargin)
}

