package project1

import java.io._

/*
 * Definition of the intermediate language targeted
 * by our parser. Seen in class.
 */
abstract class Exp
case class Lit(x: Int) extends Exp
case class Plus(x: Exp, y: Exp) extends Exp
case class Minus(x: Exp, y: Exp) extends Exp
case class Times(x: Exp, y: Exp) extends Exp
case class Div(x: Exp, y: Exp) extends Exp

// Parser
class SimpleParser(in: Reader[Char]) extends Reporter {

/*
 * Consume character 'c' if next or throw an exception
 */
  def accept(c: Char) = {
    if (in.hasNext(_ == c)) in.next()
    else expected(s"'$c'")
  }

/*
 * Test if character is a letter
 */
  def isAlpha(c: Char) =
    ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')

/*
 * Test if character is a digit
 */
  def isDigit(c: Char) = '0' <= c && c <= '9'

/*
 * Extract name from stream, or throw an exception
 * if the next character is not a letter
 */
  def getName(): String = {
    if (in.hasNext(isAlpha)) in.next().toString
    else expected("Name")
  }

/*
 * Extract number from stream, or throw an exception
 * if the next character is not a digit
 *
 * Because we want to keep things very simple, for now
 * numbers are only single digits!
 */
  def getNum(): Int = {
    if (in.hasNext(isDigit)) (in.next() - '0')
    else expected("Number")
  }
}

// Expression Parsers
/*
 * Our first parser will be very simple. It will be able to handle
 * expression that contain a single number! And in our case, numbers
 * are single digits, so you may say that it is VERY simple.
 *
 * Formally, we can define the grammar of our language as:
 * <expression> ::= <number>
 *
 * TODO: Nothing in this class
 */
class SingleDigitParser(in: Reader[Char]) extends SimpleParser(in) {

  def parseExpression: Exp = {
    println("Super duper praseExpression")
    val ret = Lit(getNum)
    println("Super duper praseExpression after")
    return ret
  }
/*
 * The code can only be made of one expression.
 */
  def parseCode: Exp = parseExpression
}

/*
 * We can improve our language with expressions of the form
 *  - 1+2
 *  - 4-3
 *  - term+/-term
 *
 * We define a new language grammar:
 * <addop>      ::= '+' | '-'
 * <term>       ::= <number>
 * <expression> ::= <term><addop><term>
 *
 * NOTE: | is read as 'or'
 *
 * We are going to write two procedures to parse code for
 * terms and expressions.
 *
 * The terms are simply the expressions of our previous language,
 * so we can reuse the code.
 *
 * In the case of the expression, we first need to parse the first term,
 * then distinguish between + or - and generate the appropriate
 * intermediate representation.
 *
 * TODO: In the Main.scala file, replace the SingleDigitParser class
 * with the following class and verify that our code works.
 * We are using two more Exps: Plus and Minus. In the Generator.scala
 * file, only Plus has been implemented for you. You need to provide
 * an implementation for Minus.
 */
class SingleAddOpParser(in: Reader[Char]) extends SingleDigitParser(in) {

  // Terms are the previous expressions
  def parseTerm: Exp = super.parseExpression

/*
 * Parse the left operand.
 * Parse add or sub.
 */
  override def parseExpression: Exp = {
    val lop = parseTerm
    in.peek match {
      case '+' => parseAdd(lop)
      case '-' => parseSub(lop)
      case _ => expected("addop")
    }
  }

  // Function to handle the addition.
  def parseAdd(lop: Exp): Exp = {
    accept('+')
    val rop = parseTerm
    println("\u001b[36mparseAdd\u001b[37m " + lop + ":" + rop)
    Plus(lop, rop)
  }

/*
 * Function to handle the subtraction.
 */
  def parseSub(lop: Exp): Exp = {
    accept('-')
    val rop = parseTerm
    println("\u001b[36mparseSub\u001b[37m " + lop + ":" + rop)
    Minus(lop, rop)
  }
}

/*
 * With our current parser, we can handle addition or
 * subtraction. However, we have lost the ability to parse
 * a single digit. You can try it and see!
 * This makes sense, as we defined legal expression as those
 * with two terms.
 *
 * Let's improve it and make the parser handle any addops.
 *
 * The new grammar:
 * <addop>      ::= '+' | '-'
 * <term>       ::= <number>
 * <expression> ::= <term>[<addop><term>]*
 *
 * NOTE: [ x ]* means that x can be repeated any number of times
 * (even 0!)
 *
 * In order to handle this language, we can add a loop
 * in the parseExpression function.
 *
 * Don't forget to test it! Use the class MultipleAddOpParser
 * in Main.scala
 *
 * Our current parser is doing a very good job at parsing valid
 * expressions. However, we have not thought a lot about invalid
 * expressions. Part of the error handling is done through
 * the 'getNum', and 'accept' methods; this is a very powerful
 * way. 'accept' manages to catch some invalid expressions that
 * are invalid by construction: 2 +1. But some invalid expressions,
 * such as 1+2 3, are not detected, and instead are handled as if
 * it was 1+2.
 * As soon as an invalid character is detected, the parser terminates
 * as though it was the end of the stream. To fix this problem, we make
 * sure that the complete code has been parsed before
 * returning.
 *
 * TODO: Replace SingleDigitParser with MultipleAddOpParser in Main.scala
 */
class MultipleAddOpParser(in: Reader[Char]) extends SingleAddOpParser(in) {

  override def parseExpression: Exp = {
    println("\u001b[36mparseExpressionStart\u001b[37m ")
    var lop = parseTerm
    println("\u001b[36mparseExpression\u001b[37m " + lop)
    while (in.hasNext(c => c == '+' || c == '-')) {
      lop = in.peek match {
        case '+' => parseAdd(lop)
        case '-' => parseSub(lop)
        case _ => expected("Addop")
      }
    }
    lop
  }

/*
* We still only accept one expression, but in addition
* we make sure that this is the end of the stream.
*/
  override def parseCode = {
    val ast = parseExpression
    if (in.hasNext)
      abort(s"Expected EOF got '${in.peek}'")
    ast
  }
}

/*
 * We are now trying to tackle the last arithmetic operations:
 * we would like to have multiplication and division.
 *
 * With Addops and Mulops, there is a problem of operator
 * precedence. In the early days of compilers, computer
 * scientists were using complex methods to enforce
 * precence rules. However, we are going to see that we can
 * do it quite easily.
 *
 * Our terms thus far have only been numbers, but in the expression
 * 1+2*4, the second term (2*4) is a product of factors
 * (2 and 4). Each factor is what our old terms were: a number.
 *
 * Our new language can be defined as:
 *
 * <addop>      ::= '+' | '-'
 * <mulop>      ::= '*' | '/'
 * <term>       ::= <factor>[<mulop><factor>]*
 * <factor>     ::= <number>
 * <expression> ::= <term>[<addop><term>]*
 *
 * Now the terms are very similar to an expression. Your job is to
 * write the new parseTerm method. We supply the implementation for
 * mul and div. The parseExpression code is the same than before.
 *
 * The operator precedence is enforced by construction. Our parser
 * starts by generating code for an expression. Therefore, it will
 * first generate code for a term. When the term is composed of
 * mulops, these operations are going to be executed first. Only
 * then will the parser handle the addop.
 *
 * NOTE: to understand in detail how a term like 1+2*3 is parsed,
 * you may find it helpful to draw the function calls and returns of
 * parseTerm, parseFactor, etc, on a sheet of paper, along with the
 * position of the Reader in the input string.
 *
 * TODO: Replace MultipleAddOpParser with ArithOpParser in Main.scala.
 * Implement the parseTerm function and update the generator to
 * handle Mul and Div.
 */
class ArithOpParser(in: Reader[Char]) extends MultipleAddOpParser(in) {

/*
 * A factor is our old term!
 */
  def parseFactor: Exp = super.parseTerm

/*
 * A term is now very similar to an expression
 */
  override def parseTerm: Exp = {
    /**
     * This will grab one expression, I will keep it here for the rest of time.
     */
    // val lop = parseFactor
    // in.peek match {
    //   case '*' => parseMul(lop)
    //   case '/' => parseDiv(lop)
    //   case _ => expected("addop")
    // }
    /**
     * Use a while loop so we can go through the whole expression instead of
     * just <term><mulop><term> , as explained above.
     */
    println("\u001b[36mparseTermfstart\u001b[37m ")
    var lop = parseFactor
    println("\u001b[36mparseTermf\u001b[37m " + lop)
    while (in.hasNext(c => c == '*' || c == '/')) {
      lop = in.peek match {
        case '*' => parseMul(lop)
        case '/' => parseDiv(lop)
        case _ => expected("Mulop")
      }
    }
    lop
  }

  def parseMul(lop: Exp): Exp = {
    println("\u001b[36mparseMul\u001b[37m")
    accept('*')
    val rop = parseFactor

    Times(lop, rop)
  }

  def parseDiv(lop: Exp): Exp = {
    println("\u001b[36mparseDiv\u001b[37m")
    accept('/')
    val rop = parseFactor
    Div(lop, rop)
  }
}

/*
 * We now have almost everything we need for a basic calculator!
 * Our final addition will be parentheses:
 *   2*(9-3*5)+3
 * Looking at this example, we can see that parentheses are used to
 * modify the operator precedence. The key idea is to realize that
 * however complex the expression enclosed within parentheses may be,
 * to the rest of the world it looks like a simple factor.
 *
 * NOTE: Our parser is not as good as we were thinking. For
 * example, try -3+4 or 4*-3. These will fail. In order to solve that
 * problem, we are adding a special case: If a factor
 * starts with a '-', it will be handle as if it was "0-factor".
 * Of course this is not the most efficient method, but without
 * adding more nodes to our intermediate representation, no much
 * better way exists.
 *
 * This gives us the final definition of our language:
 * <addop>      ::= '+' | '-'
 * <mulop>      ::= '*' | '/'
 * <term>       ::= <factor>[<mulop><factor>]*
 * <factor>     ::= ['-']<number> | ['-']'('<expression>')'
 * <expression> ::= <term>[<addop><term>]*
 *
 * NOTE: ['-'] means that '-' is optional.
 *
 * TODO: complete the parseFactor function
 */
class ArithParOpParser(in: Reader[Char]) extends ArithOpParser(in) {

  override def parseFactor: Exp = {
    in.peek match {
      case '-' =>
        accept('-')
        Minus(Lit(0), parseFactor)
      case '(' =>
        // Accept the first paren
        accept('(')
        println("( place")
        // Call my parser
        var ast = super.parseExpression
        println(") place")
        // Eat the second
        accept(')')
        ast
      case _ =>
        // If all else fails, cry and do the old stuff.
        super.parseFactor
    }
  }
}
