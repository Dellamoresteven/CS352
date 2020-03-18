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
    case H.If(MiniScalaIntP, args, ct, cf) =>
      // ???
      tempLetL(1) { c1 =>
        tempLetP(CPSAnd, Seq(args(0), c1)) { c2 =>
          L.If(CPSEq, Seq(c2, c1), ct, cf) } }
    
    case H.If(MiniScalaUnitP, args, ct, cf) =>
      // ???
      tempLetL(bitsToIntMSBF(1, 1, 1, 1)) { m =>
        tempLetL(bitsToIntMSBF(0, 0, 1, 0)) { t =>
          tempLetP(CPSAnd, Seq(args(0), m)) { r =>
            L.If(CPSEq, Seq(r, t), ct, cf) } } }
  
    case H.If(MiniScalaBoolP, args, ct, cf) =>
      // ???
      tempLetL(bitsToIntMSBF(1, 1, 1, 1)) { m =>
        tempLetL(bitsToIntMSBF(1, 0, 1, 0)) { t =>
          tempLetP(CPSAnd, Seq(args(0), m)) { r =>
            L.If(CPSEq, Seq(r, t), ct, cf) } } }
    
    case H.If(MiniScalaCharP, args, ct, cf) =>
      // ???
      tempLetL(bitsToIntMSBF(1, 1, 1)) { m =>
        tempLetL(bitsToIntMSBF(1, 1, 0)) { t =>
          tempLetP(CPSAnd, Seq(args(0), m)) { r =>
            L.If(CPSEq, Seq(r, t), ct, cf) } } }
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
      if(args.length == 1){
        tempLetL(2) { c1 =>
          // tempLetP(CPSSub, Seq(c1, args(0))) { r =>
              L.LetP(name, CPSSub, Seq(c1, args(0)), transform(body)) } 
      }else {
        tempLetP(CPSSub, args) { r =>
          tempLetL(1) { c1 =>
            L.LetP(name, CPSAdd, Seq(r, c1), transform(body)) } }
      }
  
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
     tempLetL(1) { c1 =>
        tempLetP(CPSArithShiftR, Seq(args(0), c1)) { c2 =>
          tempLetP(CPSArithShiftR, Seq(args(1), c1)) { c3 =>
            tempLetP(CPSMod, Seq(c2, c3)) { c4 =>
              tempLetP(CPSArithShiftL, Seq(c4, c1)) { c5 =>
                L.LetP(name, CPSAdd, Seq(c1, c5), transform(body)) }}}}}
      // tempLetL(1) {c1 => 
      //   tempLetL(2) {c2 => 
      //     tempLetP(CPSSub, Seq(args(0), c1) ) { c3 =>
      //       tempLetP(CPSDiv, Seq(c3, c2) ) { c4 =>
      //         tempLetP(CPSSub, Seq(args(1), c1) ) { c5 => 
      //           tempLetP(CPSDiv, Seq(c5, c2) ) { c6 => 
      //             tempLetP(CPSMod, Seq(c4, c6) ) { c7 => 
      //               tempLetP(CPSMul, Seq(c2, c7) ) { c8 => 
      //                 L.LetP(name, CPSAdd, Seq(c8, c1), transform(body)) }}}}}}}}

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
      //   tempLetP(CPSArithShiftR, Seq(args(0), c1)) { c2 =>
      //     tempLetP(CPSArithShiftR, Seq(args(1), c1)) { c3 =>
      //       tempLetP(CPSArithShiftL, Seq(c2, c3)) { c4 =>
      //         tempLetP(CPSArithShiftL, Seq(c4, c1)) { c5 =>
      //           L.LetP(name, CPSAdd, Seq(c1, c5), transform(body)) }}}}}
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
      // tempLetL(1) { c1 => 
      //   tempLetP(CPSSub, Seq(args(0), c1)) { c2 => 
      //     tempLetP(CPSArithShiftR, Seq(args(1), c1)) { c3 => 
      //       tempLetP(CPSArithShiftL, Seq(c2, c3)) { c4 => 
      //         L.LetP(name, CPSAdd, Seq(c4, c1), transform(body)) }}}}
      /* broke */
      // tempLetL(1) { c1 => 
      //   tempLetP(CPSSub, Seq(args(0), c1)) { c2 => 
      //     tempLetP(CPSArithShiftR, Seq(args(1), c1)) { c3 => 
      //       tempLetP(CPSArithShiftR, Seq(c2, c3)) { c4 => 
      //         L.LetP(name, CPSAdd, Seq(c4, c1), transform(body)) }}}}
     tempLetL(1) { c1 =>
        tempLetP(CPSArithShiftR, Seq(args(0), c1)) { c2 =>
          tempLetP(CPSArithShiftR, Seq(args(1), c1)) { c3 =>
            tempLetP(CPSArithShiftR, Seq(c2, c3)) { c4 =>
              tempLetP(CPSArithShiftL, Seq(c4, c1)) { c5 =>
                L.LetP(name, CPSAdd, Seq(c1, c5), transform(body)) }}}}}

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
      // println(s);
      L.LetC(s, transform(body))

    case H.AppC(cnts, body) =>
      L.AppC(cnts, body);

    // Functions nodes (LetF, AppF)
    // TODO
    // case H.LetF(funs, body) =>
      // println("FFFF" + funs);
      // for( i <- 0 to funs.length - 1 ) {
      //   wrap(funs(i).args, transform(funs(i).body))(_);
      // }
      // ???

    // val funDefs = functions map { f =>
    //   val rc = Symbol.fresh("rc")
    //   C.FunDef(f.name, rc, f.args map(_.name), tail(f.body, rc))
    // }
    // C.LetF(funDefs, tail(body, c))

    // case FunDef(name, retC, args, body) =>

    /** WORKING KINDA SORATA 52 */
    // case H.LetF(funs, body) =>
    //   // println("YES" + funs + "\n\n\n\n");
    //   val funDefs = funs map { f =>
    //     L.FunDef(f.name, f.retC, f.args, transform(f.body))
    //   }
    //   L.LetF(funDefs, transform(body))
    /** END WORKING */

    // case H.LetF(funs, body) =>
    //   val funDefs = funs map { f =>
    //     // var fv = freeVariables(transform(f.body))(_);
    //     // var argsList = f.args map { _ => Symbol.fresh("sc") }
    //     // argsList +: Symbol.fresh("env");
    //     var agrsList = Seq();
    //     var e = Symbol.fresh("env");
    //     agrsList :+ e;
    //     for(i <- 0 to f.args.length-1 ) {
    //       agrsList :+ Symbol.fresh("sn");
    //     }
    //     // var b = 
    //     var vstuff = Seq();
    //     // freeVariables(f.body)();
    //     // private def wrap[T](args: Seq[T], inner: L.Tree)(createLayer: (T, L.Tree) => L.Tree) = {
    //     // for(i <- 0 to f(i).args.length-1 ) {
    //       // 
    //       // var blockGetThing = tempLetL(i+1) { c1 => 
    //       //                           L.LetP( Symbol.fresh("v"), CPSBlockGet, Seq(e, c2), 
    //       //                                 wrap() ) }
    //       // vstuff :+ blockGetThing;
    //     // }
    //     // subst(???, agrsList zip vstuff);
    //     // var s1 = L.FunDef(Symbol.fresh("s"), Symbol.fresh("sc1"), argsList, b);
    //   }
    //   // L.LetF(funDefs, transform(body))
    //   ???
    
  case H.LetF(funs, body) =>
      // ???
      var s:Set[Symbol] = Set()
      var n:Set[Int] = Set()
      var ff:Set[Symbol] = Set()
      var ffgg:Set[Int] = Set()
      var pleaseHelpMe:Set[Symbol] = Set()
      val funDefs = funs map { f =>
        var fv = freeVariables(f)(Map.empty)
        val env = Symbol.fresh("env")
        val sVs = fv map { c => Symbol.fresh("v") }
        var e1 = transform(f.body)
        val sub = Substitution(f.name +: fv.toSeq, env +: sVs.toSeq)
        var subcall = e1.subst(sub)
        val ind = Seq.range(1, fv.size + 1, 1)
        val zipped = fv.toSeq zip ind
        val res = wrap(zipped, subcall) {
          case ((n, v), inner) => tempLetL(v) { c =>
              L.LetP(n, CPSBlockGet, Seq(env, c), inner)}
        }
        var w = Symbol.fresh("w")
        
        s = s + w
        s = s ++ fv
        val testMyLife = fv.size + 1;
        pleaseHelpMe = pleaseHelpMe + f.name
        ffgg = ffgg + testMyLife
        // n = n ++ Seq.range(1, fv.size + 1, 1)
        n = n ++ Seq.range(0, fv.size + 1,1).toSet
        for(i <- 0 to fv.size) {
          ff = ff + f.name
        }
        L.FunDef(w, f.retC, f.args, res)
      }
      var fff = ff zip n zip s map { 
        case ((a,b), c) => (a,b,c)
      }

      var eagfij = wrap(fff.toSeq, transform(body)) {
        case((a, b, c), d) =>
          tempLetL(b) { newB => val t = Symbol.fresh("t");
            L.LetP(t, CPSBlockSet, Seq(a, newB, c), d)
          }
      }

      var eagfijffff = wrap((pleaseHelpMe zip ffgg).toSeq, eagfij) {
        case((a, b), c) =>
          tempLetL(b) { newB =>
            L.LetP(a, CPSBlockAlloc(202), Seq(newB), c)
          }
      }
      // println(funDefs)
      // println("---------")
      // println(eagfijffff)
      L.LetF(funDefs, eagfijffff)
      // ???

    case H.AppF(name, retC, args) =>
      tempLetL(0) { c1 => 
        tempLetP(CPSBlockGet, Seq(name, c1)) { c2 => 
          L.AppF(c2, retC, name +: args) }}
          

    // case H.AppF(fun, retC, args) =>
    //   L.AppF(fun, retC, args)

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
      tempLetL(1) { c1 => 
          tempLetP(CPSArithShiftR, Seq(arg, c1)) { c2 => 
              L.Halt(c2) }}
      
  }

  private def blockGetHelperFunction(i: Int, s: String*) {
    println("HERERERERERE\n\n\n");
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
      var x = freeVariables(body);
      for( i <- 0 to funs.length - 1 ) {
          x = x ++ (freeVariables(funs(i).body) -- funs(i).args)
      }
      for( i <- 0 to funs.length - 1 ) {
        x = x - funs(i).name
      }
      x

    case H.AppC(name, args) => 
      args.toSet

    case H.AppF(fun, _, args) =>
      args.toSet + fun

    case H.If(op, args, thenC, elseC) =>
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
