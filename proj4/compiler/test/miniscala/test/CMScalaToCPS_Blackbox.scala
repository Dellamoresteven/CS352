package miniscala.test

import miniscala.test.infrastructure.CPSHighTest
import miniscala.test.ok.AllOKTests
import org.junit.Test

/** Blackbox testing for entire program outputs */
class CMScalaToCPS_Blackbox extends CPSHighTest with AllOKTests {

  val compileAndInterpret = (src: String) => testCPSHighProgramOutput(source = src)
  // TODO: Add other specific tests here

  @Test def blackBoxTestTestTestingTestSTEVE =
  compileAndInterpret("""
    |putchar(1 + 78);
    |putchar(6 + 69);
    |putchar(10)
  """.stripMargin)

  @Test def blackBoxTestTestTestingTestSTEVE2 =
  testCPSHighProgramOutput("""
    |def nl() = 10.toChar;
    |val v = "q";
    |val u = "q";
    |putchar((if (v == v) 'K' else 'O').toInt);
    |putchar(nl().toInt)
  """.stripMargin, "", "K");

  @Test def blackBoxTestsIfsInsideIfs =
  testCPSHighProgramOutput("""
    |def nl() = 10.toChar;
    |val v = "q";
    |val u = "q";
    |putchar((if (v == v) 'H' else 'Q').toInt);
    |putchar((if ( if (v == v) true else false ) 'J' else 'V').toInt);
    |putchar(nl().toInt)
  """.stripMargin, "", "HJ");

  @Test def blackBoxTestsIfsInsideIfsInsideIfs =
  testCPSHighProgramOutput("""
    |def nl() = 10.toChar;
    |val v = "q";
    |val u = "q";
    |putchar((if (v == v) 'H' else 'Q').toInt);
    |putchar((if ( if ( if (v != v) true else false ) true else false ) 'J' else 'V').toInt);
    |putchar(nl().toInt)
  """.stripMargin, "", "HV");

  @Test def blackBoxTestsIfsInsideIfsInsideIfs2 =
  testCPSHighProgramOutput("""
    |def nl() = 10.toChar;
    |val v = "q";
    |val u = "q";
    |putchar((if (v == v) 'H' else 'Q').toInt);
    |putchar((if ( if ( if (v != v) true else false ) false else true ) 'J' else 'V').toInt);
    |putchar(nl().toInt)
  """.stripMargin, "", "HJ");

  @Test def blackBoxTestsIfsInsideIfsInsideIfs3 =
  testCPSHighProgramOutput("""
    |def nl() = 10.toChar;
    |val v = "q";
    |val u = "q";
    |putchar((if (v == v) 'H' else 'Q').toInt);
    |putchar((if ( if ( if (v == v) true else false ) false else true ) 'J' else 'V').toInt);
    |putchar(nl().toInt)
  """.stripMargin, "", "HV");

  @Test def blackBoxTestsIfsInsideIfsInsideIfs4 =
  testCPSHighProgramOutput("""
    |def nl() = 10.toChar;
    |val v = "q";
    |val u = "q";
    |putchar((if (v == v) if (v == v) 'L' else 'P' else 'Q').toInt);
    |putchar((if ( if ( if (v == v) true else false ) false else true ) 'J' else 'V').toInt);
    |putchar(nl().toInt)
  """.stripMargin, "", "LV");

  @Test def blackBoxTestsIfsInsideIfsInsideIfs5 =
  testCPSHighProgramOutput("""
    |def nl() = 10.toChar;
    |val v = "q";
    |val u = "q";
    |putchar((if (v == v) if (v != v) 'L' else if(v!=u)'U' else 'I' else 'Q').toInt);
    |putchar((if ( if ( if (v == v) true else false ) false else true ) 'J' else 'V').toInt);
    |putchar(nl().toInt)
  """.stripMargin, "", "UV");

  @Test def blackBoxTestsIfThreeEqualsTwo =
  testCPSHighProgramOutput("""
    |def nl() = 10.toChar;
    |val v = 3;
    |val u = 2;
    |putchar((if (v == u) 'H' else 'Q').toInt);
    |putchar(nl().toInt)
  """.stripMargin, "", "Q");

  @Test def blackBoxTestsIfThreeNotEqualsTwo =
  testCPSHighProgramOutput("""
    |def nl() = 10.toChar;
    |val v = 3;
    |val u = 2;
    |putchar((if (v != u) 'H' else 'Q').toInt);
    |putchar(nl().toInt)
  """.stripMargin, "", "H");

  @Test def blackBoxTestsIfVarFiveEqualsFive =
  testCPSHighProgramOutput("""
    |def nl() = 10.toChar;

    |val v = 5;
    |putchar((if (v == 5) 'H' else 'Q').toInt);

    |putchar(nl().toInt)
  """.stripMargin, "", "H");

  @Test def blackBoxTestsIfVarFiveEqualsThree =
  testCPSHighProgramOutput("""
    |def nl() = 10.toChar;

    |val v = 5;
    |putchar((if (v == 3) 'H' else 'Q').toInt);

    |putchar(nl().toInt)
  """.stripMargin, "", "Q");

  @Test def blackBoxTestsWhileLoop1 =
  testCPSHighProgramOutput("""
    |def nl() = 10.toChar;

    |var v = 5;
    |while(v==5) v = v + 1; putchar('O'.toInt);

    |putchar(nl().toInt)
  """.stripMargin, "", "O");

  @Test def blackBoxTestsWhileLoop2 =
  testCPSHighProgramOutput("""
    |def nl() = 10.toChar;

    |var v = 5;
    |while(v < 100) v = v + 1; putchar(v);

    |putchar(nl().toInt)
  """.stripMargin, "", "d");

  @Test def blackBoxTestsWhileLoop3 =
  testCPSHighProgramOutput("""
    |def nl() = 10.toChar;

    |var v = 48;
    |while(v > 100) v = v + 1; putchar(v);

    |putchar(nl().toInt)
  """.stripMargin, "", "0");

  @Test def blackBoxTestsWhileLoop4 =
  testCPSHighProgramOutput("""
    |def nl() = 10.toChar;

    |var v = 38;
    |while(v < 48) {
      v = v + 1;
      putchar('H'.toInt)
    }; 
    putchar(v);

    |putchar(nl().toInt)
  """.stripMargin, "", "HHHHHHHHHH0");

  @Test def blackBoxTestsWhileLoop5 =
  testCPSHighProgramOutput("""
    |def nl() = 10.toChar;

    |var v = 38;
    |while(v < 48) {
      v = v + 1;
      if(v > 40) {
        putchar('H'.toInt)
      } else {
        putchar('N'.toInt)
      }
    }; 
    putchar(v);

    |putchar(nl().toInt)
  """.stripMargin, "", "NNHHHHHHHH0");

  @Test def blackBoxTestsWhileLoop6 =
  testCPSHighProgramOutput("""
    |def nl() = 10.toChar;

    |var v = 38;
    |while(v < 48) {
      v = v + 1;
      if(true == true) {
        putchar('H'.toInt)
      } else {
        putchar('N'.toInt)
      }
    }; 
    putchar(v);

    |putchar(nl().toInt)
  """.stripMargin, "", "HHHHHHHHHH0");

  @Test def blackBoxTestsWhileLoop7 =
  testCPSHighProgramOutput("""

    |var v = 38;
    |while(v < 48) {
      v = v + 1;
      if(true == false) {
        putchar('H'.toInt)
      } else {
        putchar('N'.toInt)
      }
    }; 
    putchar(v);

    |putchar('\n'.toInt)
  """.stripMargin, "", "NNNNNNNNNN0");

  @Test def blackBoxTestsValW1 =
  testCPSHighProgramOutput("""

    val v = 48;
    val v = 49;
    putchar(v);

    putchar('\n'.toInt)
  """.stripMargin, "", "1");

  @Test def blackBoxTestsValW2 =
  testCPSHighProgramOutput("""

    val v = 48;
    val v = 49;
    val v = 50;
    putchar(v);

    putchar('\n'.toInt)
  """.stripMargin, "", "2");

  @Test def blackBoxTestsVarW1 =
  testCPSHighProgramOutput("""

    var v = 48;
    var v = 49;
    putchar(v);

    putchar('\n'.toInt)
  """.stripMargin, "", "1");

  @Test def blackBoxTestsVarW2 =
  testCPSHighProgramOutput("""

    var v = 48;
    var v = 49;
    var v = 50;
    putchar(v);

    putchar('\n'.toInt)
  """.stripMargin, "", "2");
}
