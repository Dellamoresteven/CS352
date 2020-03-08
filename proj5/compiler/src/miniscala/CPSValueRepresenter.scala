package miniscala

import BitTwiddling.bitsToIntMSBF
import miniscala.{ SymbolicCPSTreeModule => H }
import miniscala.{ SymbolicCPSTreeModuleLow => L }

/**
 * Value-representation phase for the CPS language. Translates a tree
 * with high-level values (blocks, integers, booleans, unit) and
 * corresponding primitives to one with low-level values (blocks
 * and integers only) and corresponding primitives.
 *
 * @author Michel Schinz <Michel.Schinz@epfl.ch>
 */

object CPSValueRepresenter extends (H.Tree => L.Tree) {
  def apply(tree: H.Tree): L.Tree =
    transform(tree)(Map.empty)

  val unitLit = bitsToIntMSBF(0, 0, 1, 0)
  val optimized = false

  private def transform(tree: H.Tree)
                       (implicit worker: Map[Symbol, (Symbol, Seq[Symbol])])
      : L.Tree = tree match {

    // Literals
    case H.LetL(name, IntLit(value), body) =>
      L.LetL(name, (value << 1) | bitsToIntMSBF(1), transform(body))

    case H.LetL(name, CharLit(value), body) =>
      L.LetL(name, (value << 3) | bitsToIntMSBF(1, 1, 0), transform(body))
  
    case H.LetL(name, BooleanLit(value), body) =>
      value match {
        case true =>
          L.LetL(name, bitsToIntMSBF(1, 1, 0, 1, 0), transform(body))
        case false =>
          L.LetL(name, bitsToIntMSBF(0, 1, 0, 1, 0), transform(body))
      }

    case H.LetL(name, UnitLit, body) =>
      L.LetL(name, bitsToIntMSBF(0, 0, 1, 0), transform(body))

    // TODO: Add missing literals

    // *************** Primitives ***********************
    // Make sure you implement all possible primitives
    // (defined in MiniScalaPrimitives.scala)
    //
    // Integer primitives
    case H.LetP(name, MiniScalaIntAdd, args, body) =>
      tempLetP(CPSAdd, args) { r =>
        tempLetL(1) { c1 =>
          L.LetP(name, CPSSub, Seq(r, c1), transform(body)) } }

    case H.LetP(name, MiniScalaIntSub, args, body) =>
      tempLetP(CPSSub, args) { r =>
        tempLetL(1) { c1 =>
          L.LetP(name, CPSAdd, Seq(r, c1), transform(body)) } }
  
    // (n - 1) * (m >> 1) + 1
    case H.LetP(name, MiniScalaIntMul, args, body) =>
      tempLetL(1) {c1 => // make the vall 1 first
        tempLetP(CPSSub, Seq(args(0), c1) ) { c2 => // subtract n from vall 1
          tempLetP(CPSArithShiftR, Seq(args(1), c1)) { c3 => // m >> vall 1
            tempLetP(CPSMul, Seq(c2, c3)) { c4 => // multi the 2 things we just made
              L.LetP(name, CPSAdd, Seq(c4, c1), transform(body)) // add the 1 onto the end and keep going with the body
            } } } }

    case H.LetP(name, MiniScalaIntDiv, args, body) =>
      tempLetL(1) {c1 => // make the vall 1 first
        tempLetP(CPSSub, Seq(args(0), c1) ) { c2 => // subtract n from vall 1
          tempLetP(CPSArithShiftR, Seq(args(1), c1)) { c3 => // m >> vall 1
            tempLetP(CPSDiv, Seq(c2, c3)) { c4 => // div the 2 things we just made
              L.LetP(name, CPSAdd, Seq(c4, c1), transform(body)) // add the 1 onto the end and keep going with the body
            } } } }
    
    case H.LetP(name, MiniScalaIntMod, args, body) =>
      tempLetL(1) {c1 => 
        tempLetL(2) {c2 => 
          tempLetP(CPSSub, Seq(args(0), c1) ) { c3 =>
            tempLetP(CPSDiv, Seq(c3, c2) ) { c4 =>
              tempLetP(CPSSub, Seq(args(1), c1) ) { c5 => 
                tempLetP(CPSDiv, Seq(c5, c2) ) { c6 => 
                  tempLetP(CPSMod, Seq(c4, c6) ) { c7 => 
                    tempLetP(CPSMul, Seq(c2, c7) ) { c8 => 
                      L.LetP(name, CPSAdd, Seq(c8, c1), transform(body)) }}}}}}}}

      // tempLetP(CPSMod, args) { r =>
      //   tempLetL(0) { c1 =>
          // L.LetP(name, CPSMod, Seq(args(0), args(1)), transform(body)) 
          // } }

    // TODO: Add missing integer primitives

    // Block primitives
    // TODO: Add block primitives
    case H.LetP(name, MiniScalaBlockAlloc(x), args, body) =>
      tempLetL(1) {c1 => 
        tempLetP(CPSArithShiftR, Seq(args(0), c1)) { c2 => 
          L.LetP(name, CPSBlockAlloc(x), Seq(c2), transform(body)) }}

    case H.LetP(name, MiniScalaBlockTag, args, body) =>
      tempLetL(1) {c1 => 
        tempLetP(CPSBlockTag, Seq(args(0))) { c2 => 
          tempLetP(CPSArithShiftL, Seq(c2,c1)) { c3 =>
            L.LetP(name, CPSAdd, Seq(c3, c1), transform(body))}}}
    
    case H.LetP(name, MiniScalaBlockLength, args, body) =>
        // tempLetL(1) {c1 => 
        //   tempLetP(CPSArithShiftL, Seq(args(0), c1)) { c2 => 
        //     tempLetP(CPSAdd, Seq(args(0), c2)) { c3 => 
        //       L.LetP(name, CPSBlockLength, Seq(c3), transform(body)) }}}
        tempLetP(CPSBlockLength, args){ c1 =>
          tempLetL(1) { c2 =>
            tempLetP(CPSArithShiftL, Seq(c1,c2)) { c3 =>
              L.LetP(name, CPSAdd, Seq(c3,c2), transform(body))}}}
        // tempLetP(CPSBlockLength, args){ c1 =>
        //     L.LetL(name, c1, transform(body))}
              // L.LetP(name, CPSAdd, Seq(c3,c2), transform(body))}

    case H.LetP(name, MiniScalaBlockGet, args, body) =>
      // tempLetP(CPSBlockLength, args){ c1 =>
      //   tempLetL(1) { c2 =>
      //     tempLetP(CPSArithShiftL, Seq(c1,c2)) { c3 =>
      //       L.LetP(name, CPSAdd, Seq(c3,c2), transform(body))}}}
      // L.LetP(name, CPSBlockGet, args, transform(body))
      tempLetL(1) { c1 => 
        tempLetP(CPSArithShiftR, Seq(args(1), c1)) { c2 =>
            L.LetP(name, CPSBlockGet, Seq(args(0), c2), transform(body)) }}

    case H.LetP(name, MiniScalaBlockSet, args, body) =>
      tempLetL(1) { c1 => 
        tempLetP(CPSArithShiftR, Seq(args(1), c1)) { c2 =>
            L.LetP(name, CPSBlockSet, Seq(args(0), c2, args(2)), transform(body)) }}

    // Conversion primitives int->char/char->int
    // TODO
    case H.LetP(name, MiniScalaCharToInt, args, body) =>
      tempLetL(2) { c1 => 
        L.LetP(name, CPSArithShiftR, Seq(args(0), c1), transform(body)) }

    case H.LetP(name, MiniScalaIntToChar, args, body) =>
      tempLetL(2) { c1 => 
        tempLetP(CPSArithShiftL, Seq(args(0), c1)) { c2 => 
          L.LetP(name, CPSAdd, Seq(c2, c1), transform(body)) }}

    // IO primitives
    // TODO

    case H.LetP(name, MiniScalaByteRead, args, body) =>
      tempLetL(1) { c1 => 
        tempLetP(CPSByteRead, Seq()) { c2 => 
          tempLetP(CPSArithShiftL, Seq(c2, c1)) { c3 => 
            L.LetP(name, CPSAdd, Seq(c3, c1), transform(body)) }}}

    case H.LetP(name, MiniScalaByteWrite, args, body) =>
      tempLetL(1) { c1 => 
          tempLetP(CPSArithShiftR, Seq(args(0), c1)) { c2 => 
            L.LetP(name, CPSByteWrite, Seq(c2), transform(body)) }}

    // Other primitives
    // TODO
    case H.LetP(name, MiniScalaId, args, body) =>
      L.LetP(name, CPSId, args, transform(body))

    case H.LetP(name, MiniScalaIntArithShiftLeft, args, body) =>
      tempLetL(1) { c1 => 
        tempLetP(CPSSub, Seq(args(0), c1)) { c2 => 
          tempLetP(CPSArithShiftR, Seq(args(1), c1)) { c3 => 
            tempLetP(CPSArithShiftL, Seq(c2, c3)) { c4 => 
              L.LetP(name, CPSAdd, Seq(c4, c1), transform(body)) }}}}
      // tempLetL(1) { c1 => 
      //   tempLetL(2) { c2 => 
      //     tempLetP(CPSSub, Seq(args(0), c1)) { c3 => 
      //       tempLetP(CPSDiv, Seq(c3, c2)) { c4 => // d
      //         tempLetP(CPSSub, Seq(args(1), c1)) { c5 => 
      //           tempLetP(CPSDiv, Seq(c5, c2)) { c6 => 
      //             tempLetP(CPSArithShiftL, Seq(c4, c6)) { c7 => 
      //               tempLetP(CPSMul, Seq(c2, c7)) { c8 => 
      //               L.LetP(name, CPSAdd, Seq(c8, c1), transform(body)) }}}}}}}}

    case H.LetP(name, MiniScalaIntArithShiftRight, args, body) =>
      tempLetL(1) { c1 => 
        tempLetP(CPSSub, Seq(args(0), c1)) { c2 => 
          tempLetP(CPSArithShiftR, Seq(args(1), c1)) { c3 => 
            tempLetP(CPSArithShiftR, Seq(c2, c3)) { c4 => 
              L.LetP(name, CPSAdd, Seq(c4, c1), transform(body)) }}}}

    case H.LetP(name, MiniScalaIntBitwiseAnd, args, body) =>
      // tempLetL(1) { c1 => 
        // tempLetP(CPSAnd, Seq(args(0), args(1))) { c2 => 
          L.LetP(name, CPSAnd, Seq(args(0),args(1)), transform(body)) 

    case H.LetP(name, MiniScalaIntBitwiseOr, args, body) =>
      // tempLetL(1) { c1 => 
      //   tempLetP(CPSAnd, Seq(args(0), args(1))) { c2 => 
          L.LetP(name, CPSOr, Seq(args(0), args(1)), transform(body)) 

    case H.LetP(name, MiniScalaIntBitwiseXOr, args, body) =>
      // tempLetL(1) { c1 => 
      //   tempLetP(CPSAnd, Seq(args(0), args(1))) { c2 => 
      tempLetL(1) { c1 => 
        tempLetP(CPSXOr, Seq(args(0), args(1))) { c2 => 
          L.LetP(name, CPSAdd, Seq(c2, c1), transform(body)) }}

    // Continuations nodes (LetC, AppC)
    // TODO
    // val substCnts = cnts map {
    //   case CntDef(name, args, body) =>
    //     CntDef(s(name), args map s, substIn(body))
    // }
    
    case H.LetC(cnts, body) =>
      var s : List[L.CntDef] = List();
      for(i <- 0 to cnts.length - 1) {
        s = s :+ L.CntDef(cnts(i).name, cnts(i).args, transform(cnts(i).body))
      }
      println(s);
      L.LetC(s, transform(body))

    case H.AppC(cnts, body) =>
      L.AppC(cnts, body);

    // Functions nodes (LetF, AppF)
    // TODO
    case H.LetF(funs, body) =>
      println("FFFF" + funs);
      for( i <- 0 to funs.length - 1 ) {
        wrap(funs(i).args, transform(funs(i).body))(_);
      }
      ???

    case H.AppF(fun, retC, args) =>
      L.AppF(fun, retC, args)

    // ********************* Conditionnals ***********************
    // Type tests
    case H.If(MiniScalaBlockP, Seq(a), thenC, elseC) =>
      ifEqLSB(a, Seq(0, 0), thenC, elseC)
    // TODO: add missing cases

    // Test primitives (<, >, ==, ...)
    // TODO
    // case H.LetP(name, MiniScalaIntLt, args, body) =>
    //   // L.LetP(name, CPSLt, Seq(args(0), args(1)), transform(body))
    //   ???
    // case H.LetC(args, body) =>
    //   // println(args(0));
    //   L.LetC(Seq(args(0), args(1)), transform(body))

    case H.If(MiniScalaIntLt, args, thenC, elseC) =>
      L.If(CPSLt, args, thenC, elseC)

    case H.If(MiniScalaIntLe, args, thenC, elseC) =>
      L.If(CPSLe, args, thenC, elseC)

    case H.If(MiniScalaEq, args, thenC, elseC) =>
      L.If(CPSEq, args, thenC, elseC)

    case H.If(MiniScalaNe, args, thenC, elseC) =>
      L.If(CPSNe, args, thenC, elseC)

    case H.If(MiniScalaIntGt, args, thenC, elseC) =>
      L.If(CPSGt, args, thenC, elseC)

    case H.If(MiniScalaIntGe, args, thenC, elseC) =>
      L.If(CPSGe, args, thenC, elseC)

    // Halt case
    case H.Halt(arg) =>
      L.Halt(arg);
  }

  /*
   * Auxilary function.
   *
   * Example:
   *  // assuming we have a function with symbol f and the return continuation is rc:
   *
   *  val names = Seq("first", "second")
   *  val values = Seq(42, 112)
   *  val inner = L.AppF(f, rc, names)
   *  val res = wrap(names zip values , inner) {
   *    case ((n, v), inner) => L.LetL(n, v, inner)
   *  }
   *
   *  // res is going to be the following L.Tree
   *  L.LetL("first", 42,
   *    L.LetL("second", 112,
   *      L.AppF(f, rc, Seq("first", "second"))
   *    )
   *  )
   */
  private def wrap[T](args: Seq[T], inner: L.Tree)(createLayer: (T, L.Tree) => L.Tree) = {
    def addLayers(args: Seq[T]): L.Tree = args match {
      case h +: t => createLayer(h, addLayers(t))
      case _ => inner
    }
    addLayers(args)
  }

  private def freeVariables(tree: H.Tree)
                           (implicit worker: Map[Symbol, Set[Symbol]])
      : Set[Symbol] = tree match {
    case H.LetL(name, _, body) =>
      freeVariables(body) - name
    case H.LetP(name, _, args, body) =>
      freeVariables(body) - name ++ args
    case H.LetC(cont, body) =>
      val x = freeVariables(body);
      for( i <- 0 to cont.length - 1 ) {
          x ++ (freeVariables(cont(i)) -- cont(i).args)
      }
      x

    case H.LetF(funs, body) =>
      val x = freeVariables(body);
      for( i <- 0 to funs.length - 1 ) {
          x ++ (freeVariables(funs(i)) -- funs(i).args)
      }
      for( i <- 0 to funs.length - 1 ) {
        x - funs(i).name
      }
      x

    case H.AppC(args, body) =>
      args

    case H.AppF(fun, _, args) =>
      fun ++ args

    case H.If(op, args, thenC, elseC) =>
    // val s : type args = Set()
    // // for(i <- 0 to args.length - 1){
    // //   s += args(i)
    // // }
    // s
    args.toSet;
      // ???
    // TODO: add missing cases
  }

  private def freeVariables(cnt: H.CntDef)
                           (implicit worker: Map[Symbol, Set[Symbol]])
      : Set[Symbol] =
    freeVariables(cnt.body) -- cnt.args

  private def freeVariables(fun: H.FunDef)
                           (implicit worker: Map[Symbol, Set[Symbol]])
      : Set[Symbol] =
    freeVariables(fun.body) - fun.name -- fun.args

  // Tree builders

  /**
   * Call body with a fresh name, and wraps its resulting tree in one
   * that binds the fresh name to the given literal value.
   */
  private def tempLetL(v: Int)(body: L.Name => L.Tree): L.Tree = {
    val tempSym = Symbol.fresh("t")
    L.LetL(tempSym, v, body(tempSym))
  }

  /**
   * Call body with a fresh name, and wraps its resulting tree in one
   * that binds the fresh name to the result of applying the given
   * primitive to the given arguments.
   */
  private def tempLetP(p: L.ValuePrimitive, args: Seq[L.Name])
                      (body: L.Name => L.Tree): L.Tree = {
    val tempSym = Symbol.fresh("t")
    L.LetP(tempSym, p, args, body(tempSym))
  }

  /**
   * Generate an If tree to check whether the least-significant bits
   * of the value bound to the given name are equal to those passed as
   * argument. The generated If tree will apply continuation tC if it
   * is the case, and eC otherwise. The bits should be ordered with
   * the most-significant one first (e.g. the list (1,1,0) represents
   * the decimal value 6).
   */
  private def ifEqLSB(arg: L.Name, bits: Seq[Int], tC: L.Name, eC: L.Name)
      : L.Tree =
    tempLetL(bitsToIntMSBF(bits map { b => 1 } : _*)) { mask =>
      tempLetP(CPSAnd, Seq(arg, mask)) { masked =>
        tempLetL(bitsToIntMSBF(bits : _*)) { value =>
          L.If(CPSEq, Seq(masked, value), tC, eC) } } }
}
