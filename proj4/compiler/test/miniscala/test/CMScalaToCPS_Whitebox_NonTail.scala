package miniscala.test

import ok.AllOKTests

import miniscala.test.infrastructure.CPSHighTest
import org.junit.Test

/** Whitebox testing for entire program outputs */
class CMScalaToCPS_Whitebox_NonTail extends CPSHighTest {

  // TODO: Test recursive functions

  @Test def testNonTailLiteral = {
    testCPSHighTreeEquality("3", "vall v$1 = 3; vall v$2 = 0; halt(v$2)")
  } 

  @Test def testNonTailMultiLet =
    testCPSHighTreeEquality("val x = 1; val y = 2; y",
        "vall v$1 = 1; valp v$2 = id(v$1); vall v$3 = 2; valp v$4 = id(v$3); vall v$5 = 0; halt(v$5)")

  // TODO add more tests
  @Test def testNonTailMultiVarDec =
    testCPSHighTreeEquality("var x = 1; var y = 2; y",
        "vall v$1 = 1; valp v$2 = block-alloc-242(v$1); vall v$3 = 0; vall v$4 = 1; valp v$5 = block-set(v$2, v$3, v$4); vall v$6 = 1; valp v$7 = block-alloc-242(v$6); vall v$8 = 0; vall v$9 = 2; valp v$10 = block-set(v$7, v$8, v$9); vall v$11 = 0; valp v$12 = block-get(v$7, v$11); vall v$13 = 0; halt(v$13)")

  @Test def testNonTailMultiVarDec2 =
    testCPSHighTreeEquality("var x = 1; var y = 2; 3", """
        vall v$1 = 1;
        valp v$2 = block-alloc-242(v$1);
        vall v$3 = 0;
        vall v$4 = 1;
        valp v$5 = block-set(v$2, v$3, v$4);
        vall v$6 = 1;
        valp v$7 = block-alloc-242(v$6);
        vall v$8 = 0;
        vall v$9 = 2;
        valp v$10 = block-set(v$7, v$8, v$9);
        vall v$11 = 3;
        vall v$12 = 0;
        halt(v$12)
      """.stripMargin)

  @Test def testNonTailMultiVarDecIf =
    testCPSHighTreeEquality("var x = 1; var y = 1; if(x == y) 3 else 5", """
        vall v$1 = 1;
        valp v$2 = block-alloc-242(v$1);
        vall v$3 = 0;
        vall v$4 = 1;
        valp v$5 = block-set(v$2, v$3, v$4);
        vall v$6 = 1;
        valp v$7 = block-alloc-242(v$6);
        vall v$8 = 0;
        vall v$9 = 1;
        valp v$10 = block-set(v$7, v$8, v$9);
        defc v$11(v$12) = { vall v$13 = 0; halt(v$13) };
        defc v$14() = { vall v$15 = 3; v$11(v$15) };
        defc v$16() = { vall v$17 = 5; v$11(v$17) };
        vall v$18 = 0;
        valp v$19 = block-get(v$2, v$18);
        vall v$20 = 0;
        valp v$21 = block-get(v$7, v$20);
        if (v$19 == v$21) v$14 else v$16
      """.stripMargin)

  @Test def testNonTailMultiVarDecNotIf =
    testCPSHighTreeEquality("var x = 1; var y = 1; if(x != y) 3 else 5", """
      vall v$1 = 1;
      valp v$2 = block-alloc-242(v$1);
      vall v$3 = 0;
      vall v$4 = 1;
      valp v$5 = block-set(v$2, v$3, v$4);
      vall v$6 = 1;
      valp v$7 = block-alloc-242(v$6);
      vall v$8 = 0;
      vall v$9 = 1;
      valp v$10 = block-set(v$7, v$8, v$9);
      defc v$11(v$12) = { vall v$13 = 0; halt(v$13) };
      defc v$14() = { vall v$15 = 3; v$11(v$15) };
      defc v$16() = { vall v$17 = 5; v$11(v$17) };
      vall v$18 = 0;
      valp v$19 = block-get(v$2, v$18);
      vall v$20 = 0;
      valp v$21 = block-get(v$7, v$20);
      if (v$19 != v$21) v$14 else v$16
      """.stripMargin)

  @Test def testNonTailWhile =
    testCPSHighTreeEquality("var x = 1; var y = 5; while(x < y){x = x + 2; y = y + 1}; x", """
      vall v$1 = 1;
      valp v$2 = block-alloc-242(v$1);
      vall v$3 = 0;
      vall v$4 = 1;
      valp v$5 = block-set(v$2, v$3, v$4);
      vall v$6 = 1;
      valp v$7 = block-alloc-242(v$6);
      vall v$8 = 0;
      vall v$9 = 5;
      valp v$10 = block-set(v$7, v$8, v$9);
      defc v$11(v$12) = {
        defc v$13() = {
          vall v$14 = 0;
          valp v$15 = block-get(v$2, v$14);
          vall v$16 = 0;
          halt(v$16)
        };
        defc v$17() = {
          vall v$18 = 0;
          vall v$19 = 0;
          valp v$20 = block-get(v$2, v$19);
          vall v$21 = 2;
          valp v$22 = v$20 + v$21;
          valp v$23 = block-set(v$2, v$18, v$22);
          valp v$24 = id(v$22);
          vall v$25 = 0;
          vall v$26 = 0;
          valp v$27 = block-get(v$7, v$26);
          vall v$28 = 1;
          valp v$29 = v$27 + v$28;
          valp v$30 = block-set(v$7, v$25, v$29);
          valp v$31 = id(v$29);
          vall v$32 = ();
          v$11(v$32)
        };
        vall v$33 = 0;
        valp v$34 = block-get(v$2, v$33);
        vall v$35 = 0;
        valp v$36 = block-get(v$7, v$35);
        if (v$34 < v$36) v$17 else v$13
      };
      vall v$37 = ();
      v$11(v$37)
      """.stripMargin)

  @Test def testNonTailWhileInsideIf =
    testCPSHighTreeEquality("var x = 1; var y = 5; if(1 == 1) {while(x < y){x=x+2; y=y+1};x} else {5}", """
        vall v$1 = 1;
        valp v$2 = block-alloc-242(v$1);
        vall v$3 = 0;
        vall v$4 = 1;
        valp v$5 = block-set(v$2, v$3, v$4);
        vall v$6 = 1;
        valp v$7 = block-alloc-242(v$6);
        vall v$8 = 0;
        vall v$9 = 5;
        valp v$10 = block-set(v$7, v$8, v$9);
        defc v$11(v$12) = { vall v$13 = 0; halt(v$13) };
        defc v$14() = {
          defc v$15(v$16) = {
            defc v$17() = {
              vall v$18 = 0;
              valp v$19 = block-get(v$2, v$18);
              v$11(v$19)
            };
            defc v$20() = {
              vall v$21 = 0;
              vall v$22 = 0;
              valp v$23 = block-get(v$2, v$22);
              vall v$24 = 2;
              valp v$25 = v$23 + v$24;
              valp v$26 = block-set(v$2, v$21, v$25);
              valp v$27 = id(v$25);
              vall v$28 = 0;
              vall v$29 = 0;
              valp v$30 = block-get(v$7, v$29);
              vall v$31 = 1;
              valp v$32 = v$30 + v$31;
              valp v$33 = block-set(v$7, v$28, v$32);
              valp v$34 = id(v$32);
              vall v$35 = ();
              v$15(v$35)
            };
            vall v$36 = 0;
            valp v$37 = block-get(v$2, v$36);
            vall v$38 = 0;
            valp v$39 = block-get(v$7, v$38);
            if (v$37 < v$39) v$20 else v$17
          };
          vall v$40 = ();
          v$15(v$40)
        };
        defc v$41() = { vall v$42 = 5; v$11(v$42) };
        vall v$43 = 1;
        vall v$44 = 1;
        if (v$43 == v$44) v$14 else v$41
      """.stripMargin)

  @Test def testNonTailValAndVarWhile =
    testCPSHighTreeEquality("val x = 1; var y = 5; while(x < 5){val x = x + 2; y = y + 1}; x", """
      vall v$1 = 1;
      valp v$2 = id(v$1);
      vall v$3 = 1;
      valp v$4 = block-alloc-242(v$3);
      vall v$5 = 0;
      vall v$6 = 5;
      valp v$7 = block-set(v$4, v$5, v$6);
      defc v$8(v$9) = {
        defc v$10() = { vall v$11 = 0; halt(v$11) };
        defc v$12() = {
          vall v$13 = 2;
          valp v$14 = v$2 + v$13;
          valp v$15 = id(v$14);
          vall v$16 = 0;
          vall v$17 = 0;
          valp v$18 = block-get(v$4, v$17);
          vall v$19 = 1;
          valp v$20 = v$18 + v$19;
          valp v$21 = block-set(v$4, v$16, v$20);
          valp v$22 = id(v$20);
          vall v$23 = ();
          v$8(v$23)
        };
        vall v$24 = 5;
        if (v$2 < v$24) v$12 else v$10
      };
      vall v$25 = ();
      v$8(v$25)
      """.stripMargin)
}