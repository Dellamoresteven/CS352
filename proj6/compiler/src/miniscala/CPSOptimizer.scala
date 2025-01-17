
package miniscala

import scala.collection.mutable.{ Map => MutableMap }

abstract class CPSOptimizer[T <: CPSTreeModule { type Name = Symbol }]
  (val treeModule: T) {
  import treeModule._

  def apply(tree: Tree): Tree = {
    val simplifiedTree = fixedPoint(tree)(shrink)
    val maxSize = (size(simplifiedTree) * 1.5).toInt
    fixedPoint(simplifiedTree, 8) { t => inline(t, maxSize) }
  }

  /* Counts how many times a symbol is encountered as an applied function,
   * and how many as a value
   */
  private case class Count(applied: Int = 0, asValue: Int = 0)

  /* Local state of the optimization
   * Note: To update the state, use the with* methods
   */
  private case class State(
    /* How many times a symbol is encountered in the Tree. Note: The
     * census for the whole program gets calculated once in the
     * beginning, and passed to the initial state.
     */
    census: Map[Name, Count],
    // Name substitution that needs to be applied to the current tree
    subst: Substitution[Name] = Substitution.empty,
    // Names that have a constant value
    lEnv: Map[Name, Literal] = Map.empty,
    // The inverse of lEnv
    lInvEnv: Map[Literal, Name] = Map.empty,
    // A known block mapped to its tag and length
    bEnv: Map[Name, (Literal, Name)] = Map.empty,
    // ((p, args) -> n2) is included in eInvEnv iff n2 == p(args)
    // Note: useful for common-subexpression elimination
    eInvEnv: Map[(ValuePrimitive, Seq[Name]), Name] = Map.empty,
    // Continuations that will be inlined
    cEnv: Map[Name, CntDef] = Map.empty,
    // Functions that will be inlined
    fEnv: Map[Name, FunDef] = Map.empty) {

    // Checks whether a symbol is dead in the current state
    def dead(s: Name): Boolean =
      census get s map (_ == Count(applied = 0, asValue = 0)) getOrElse true
    // Checks whether a symbols is applied exactly once as a function
    // in the current State, and never used as a value
    def appliedOnce(s: Name): Boolean =
      census get s map (_ == Count(applied = 1, asValue = 0)) getOrElse false

    // Addas a substitution to the state
    def withSubst(from: Name, to: Name): State =
      copy(subst = subst + (from -> to))
    // Adds a Seq of substitutions to the state
    def withSubst(from: Seq[Name], to: Seq[Name]): State =
      copy(subst = subst ++ (from zip to))
    // Adds a constant to the State
    def withLit(name: Name, value: Literal) =
      copy(lEnv = lEnv + (name -> value), lInvEnv = lInvEnv + (value -> name))
    // Adds a block to the state
    def withBlock(name: Name, tag: Literal, size: Name) =
      copy(bEnv = bEnv + (name -> (tag, size)))
    // Adds a primitive assignment to the state
    def withExp(name: Name, prim: ValuePrimitive, args: Seq[Name]) =
      copy(eInvEnv = eInvEnv + ((prim, args) -> name))
    // Adds an inlinable continuation to the state
    def withCnt(cnt: CntDef) =
      copy(cEnv = cEnv + (cnt.name -> cnt))
    // Adds a Seq of inlinable continuations to the state
    def withCnts(cnts: Seq[CntDef]) =
      (this /: cnts) (_.withCnt(_))
    // Adds an inlinable function to the state
    def withFun(fun: FunDef) =
      copy(fEnv = fEnv + (fun.name -> fun))
    // Adds a Seq of inlinable functions to the state
    def withFuns(funs: Seq[FunDef]) =
      (this /: funs) (_.withFun(_))
    /*
     * The same state, with emply inverse environments.
     * Use this when entering a new FunDef, because assigned Name's may
     * come out of scope during hoisting.
     */
    def withEmptyInvEnvs =
      copy(lInvEnv = Map.empty, eInvEnv = Map.empty)
  }

  private def checkIfArgsExist (args : Seq[Name])(s : State) : Boolean = {
    var e = true
    for(i <- 0 to args.length - 1) {
      if (!s.lEnv.contains(args(i))) {
        e = false
      }
    }
    e
  }
  // Shrinking optimizations

  private def shrink(tree: Tree): Tree = {
    def shrinkT(tree: Tree)(implicit s: State): Tree = tree match {
      case LetL(name, value, body) if s.dead(name) => 
        shrinkT(body)

      // case LetL(name, value, body) if (s.lEnv contains name) =>
      //   shrinkT(body.subst(Substitution(name, s.lInvEnv(value))))

      case LetL(name, value, body) if (s.lInvEnv contains value) =>
        shrinkT(body.subst(Substitution(name, s.lInvEnv(value))))

      case LetL(name, value, body) =>
        LetL(name, value, shrinkT(body)(s.withLit(name, value)))

      case LetP(name, operation, arg, body) if (operation == identity) =>
        // println("LetP ID Name: " + name + " : " + arg(0))
        shrinkT(body.subst(Substitution(name, arg(0))))

      case LetP(name, operation, args, body) if blockAllocTag.isDefinedAt(operation) =>
        val newState = s.withBlock(name, blockAllocTag(operation), args(0))
        LetP(name, operation, args , shrinkT(body)(newState))

      case LetP(name, operation, args, body) if (unstable(operation)) =>
        val nargs = args map { arg => s.subst(arg) } 
        LetP(name, operation, nargs, shrinkT(body))

      case LetP(name, operation, args, body) if operation == blockTag =>
        if(s.bEnv.isDefinedAt(args(0))) {
          // println("FEAW " + s.bEnv(args(0))._2)
          shrinkT(LetL(name, s.bEnv(args(0))._1, body))
        } else {
          LetP(name, operation, args, shrinkT(body))
        }

      // neutral left
      case LetP(name, operation, Seq(x, y), body) if s.lEnv.get(x).exists(arg => leftNeutral(arg -> operation)) =>
        // println("WHERE")
        shrinkT(body.subst(Substitution(name, y)))

      // neutral right
      case LetP(name, operation, Seq(x, y), body) if s.lEnv.get(y).exists(arg => rightNeutral(operation -> arg)) =>
        // println("WHERE")
        shrinkT(body.subst(Substitution(name, x)))

      // Absorbing left
      case LetP(name, operation, Seq(x, y), body) if s.lEnv.get(x).exists(arg => leftAbsorbing(arg -> operation)) =>
        shrinkT(body.subst(Substitution(name, x)))

      // Absorbing right
      case LetP(name, operation, Seq(x, y), body) if s.lEnv.get(y).exists(arg => rightAbsorbing(operation -> arg)) =>
        shrinkT(body.subst(Substitution(name, y)))

      //https://www.scala-lang.org/api/2.9.3/scala/PartialFunction.html
      case LetP(name, operation, Seq(x,y), body) if (x == y && sameArgReduce.isDefinedAt(operation)) =>
        val v = sameArgReduce(operation)
        LetL(name, v, shrinkT(body)(s.withLit(name, v)))

      /* run "val x = 5; val y = 3; val z = x + y; putchar(z+65); 4" */
      case LetP(name, operation, args, body) if (checkIfArgsExist(args)(s)) && !impure(operation) && !unstable(operation) =>
        val v = vEvaluator((operation, args map { arg => s.lEnv(arg) }))
        LetL(name, v, shrinkT(body)(s.withLit(name, v)))

      case LetP(name, operation, args, body) if s.dead(name) && !impure(operation) =>
        // println("LetP DEAD Name: " + name)
        shrinkT(body)

      // case LetP(name, operation, args, body) if s.eInvEnv contains (operation, args) =>
      //   val newName = s.eInvEnv((operation, args))
      //   shrinkT(body.subst(Substitution(name, newName)))

      case LetP(name, operation, args, body) =>
        // println("LetP ALIVE Name: " + name)
        LetP(name, operation, args, shrinkT(body)(s.withExp(name, operation, args)))

      case If(cond, Seq(x,y), thenC, elseC) if (x == y) =>
        if (sameArgReduceC(cond))
          AppC(thenC, Seq())
        else
          AppC(elseC, Seq())

      case If(cond, args, ct, cf) if (checkIfArgsExist(args)(s)) =>
          if(cEvaluator(cond, args map {x => s.lEnv(x)})) {
            AppC(ct, Seq())
          } else {
            AppC(cf, Seq())
          }

      // case LetF(funs, body) =>
      //   if(funs.length == 0){
      //     shrinkT(body)
      //   } else if (funs.exists(f => s.dead(f.name))) { // remove dead stuff
      //     var retF = funs filter { case FunDef(name, _, _, _) => !s.dead(name) }
      //     shrinkT(LetF(retF, body))
      //   } else if(funs.exists(f => s.appliedOnce(f.name))) { // check if the function is called ONCE
      //     var inlined = funs filter { case FunDef(name, _, _, _) => s.appliedOnce(name) }
      //     var whyDoesThisNotWork = funs filter { case FunDef(name, _, _, _) => !s.appliedOnce(name) }
      //     // println("\n\n\nINLINE\n" + inlined)
      //     // println("\n\n\nNOPE" + whyDoesThisNotWork)
      //     // val st = s.withEmptyInvEnvs.withFuns(inlined)
      //     val f = whyDoesThisNotWork map {
      //         case FunDef(name, retC, args, fbd) => FunDef(name, retC, args, shrinkT(fbd)(s.withEmptyInvEnvs.withFuns(inlined)))
      //     }
      //     // shrinkT(LetF(whyDoesThisNotWork, body))(s.withFuns(inlined))
      //     LetF(f, shrinkT(body)(s.withFuns(whyDoesThisNotWork)))
      //   } else { // if the function is not dead and is called more then once
      //     val optimizedFunctions = 
      //       funs map { case FunDef(name, r, a, b) => FunDef(name, r, a, shrinkT(b)) }
      //     LetF(optimizedFunctions, shrinkT(body))
      //   }

      // default
      case LetF(funs, body) =>
        if(funs.length == 0){
          shrinkT(body)
        } else if (funs.exists(f => s.dead(f.name))) { // remove dead stuff
          var retF = funs filter { case FunDef(name, _, _, _) => !s.dead(name) }
          shrinkT(LetF(retF, body))
        } else {
          var inlineFunctions = funs filter { case FunDef(name, _, _, _) => s.appliedOnce(name) }
          var notInlineFunctions = funs filter { case FunDef(name, _, _, _) => !s.appliedOnce(name) }
          val f = notInlineFunctions map {
              case FunDef(name, retC, args, fbd) => FunDef(name, retC, args, shrinkT(fbd)(s.withEmptyInvEnvs.withFuns(inlineFunctions)))
          }
          LetF(f, shrinkT(body)(s.withFuns(inlineFunctions)))
        }


      case LetC(cnt, body) =>
        if(cnt.length == 0){
          shrinkT(body)
        } else if (cnt.exists(f => s.dead(f.name))) { // remove dead stuff
          var retC = cnt filter { case CntDef(name, _, _) => !s.dead(name) }
          shrinkT(LetC(retC, body))
        } else {
          var inlineCnts = cnt filter { case CntDef(name, _, _) => s.appliedOnce(name) }
          var notInlineCnts = cnt filter { case CntDef(name, _, _) => !s.appliedOnce(name) }
          val f = notInlineCnts map {
              case CntDef(name, args, fbd) => CntDef(name, args, shrinkT(fbd)(s.withEmptyInvEnvs.withCnts(inlineCnts)))
          }
          LetC(f, shrinkT(body)(s.withCnts(inlineCnts)))
        }
      // // default
      // case LetC(cnt, body) =>
      //   // println("cnt:" + cnt)
      //   if(cnt.length == 0){
      //     shrinkT(body)
      //   } else if (cnt.exists(f => s.dead(f.name))) { // remove dead stuff
      //     var retF = cnt filter { case CntDef(name, _, _) => !s.dead(name) }
      //     shrinkT(LetC(retF, body))
      //   } else if(cnt.exists(f => s.appliedOnce(f.name))) { // check if the function is called ONCE
      //     var inlined = cnt filter { case CntDef(name, _, _) => s.appliedOnce(name) }
      //     var whyDoesThisNotWork = cnt filter { case CntDef(name, _, _) => !s.appliedOnce(name) }
      //     // println("\n\n\nINLINE\n" + inlined)
      //     // println("\n\n\nNOPE" + whyDoesThisNotWork)
      //     val st = s.withEmptyInvEnvs.withCnts(inlined)
      //     shrinkT(LetC(whyDoesThisNotWork, body))(st)
      //   } else { // if the function is not dead and is called more then once
      //     val optimizedFunctions = 
      //       cnt map { case CntDef(name, r, a) => CntDef(name, r, shrinkT(a)) }
      //     LetC(optimizedFunctions, shrinkT(body))
      //   }

      case AppF(fun, retC, args) =>
        if (s.fEnv contains fun) {
          val FunDef(_, b, c, d) = s.fEnv(fun)
          shrinkT(d.subst(Substitution(b +: c, retC +: args)))
          // shrinkT(d)(s.withSubst(b +: c, retC +: args))
        } else {
          tree 
        }

      case AppC(cnt, args) =>
        if (s.cEnv contains cnt) {
          val CntDef(_, a, b) = s.cEnv(cnt)
          shrinkT(b.subst(Substitution(a, args)))
        } else {
          tree 
        }

      case _ =>
        // println("Default: ")
        tree
    }

    shrinkT(tree)(State(census(tree)))
  }


  // (Non-shrinking) inlining

  private def inline(tree: Tree, maxSize: Int): Tree = {

    val fibonacci = Seq(1, 2, 3, 5, 8, 13)

    val trees = Stream.iterate((0, tree), fibonacci.length) { case (i, tree) =>
      val funLimit = fibonacci(i)
      val cntLimit = i

      def inlineT(tree: Tree)(implicit s: State): Tree = tree match {
        case LetL(name, value, body) =>
          val newName = Symbol.fresh("l")
          // println("LetL2: " + newName+ " : " + value + "\n") 
          val newBody = inlineT(body.subst(Substitution(name, newName)))
          LetL(newName, value, newBody)
        case LetP(name, operation, args, body) => 
          val newName = Symbol.fresh("p")
          // println("LetP1: " + newName + " : " + newName + "\n") 
          val newBody = inlineT(body.subst(Substitution(name, newName)))
          LetP(newName, operation, args, newBody)
        case LetF(funs, body) =>
          var inlineF = funs filter { case FunDef(name, _, _, bodyy) => (size(bodyy) <= funLimit && !(s.fEnv contains name)) }
          inlineF = inlineF filter { case FunDef(name, _, _, _) => !s.dead(name) }
          val newfuns = funs map { case FunDef(name, retC, args, bodyy) => FunDef(name, retC, args, inlineT(bodyy)) }
          var ns = s.withEmptyInvEnvs
          LetF(newfuns, inlineT(body)(ns.withFuns(inlineF)))
        case LetC(cnts, body) =>
          var inlineC = cnts filter { case CntDef(name, _, bodyy) => (size(bodyy) <= cntLimit && !(s.cEnv contains name)) }
          inlineC = inlineC filter { case CntDef(name, _, _) => !s.dead(name) }
          val newcnts = cnts map { case CntDef(name, args, bodyy) => CntDef(name, args, inlineT(bodyy)) }
          var ns = s.withEmptyInvEnvs
          LetC(newcnts, inlineT(body)(ns.withCnts(inlineC)))
        case AppC(cnt, args) =>
          if (s.cEnv contains cnt) {
            val CntDef(_, a, b) = s.cEnv(cnt)
            inlineT(b.subst(Substitution(a, args)))
          } else {
            tree 
          }
        case AppF(fun, retC, args) =>
          if (s.fEnv contains fun) {
            val FunDef(_, b, c, d) = s.fEnv(fun)
            var m:Map[Symbol, Symbol] = Map()
            for(arg <- args){
              m += (arg -> arg)
            }
            m += (retC -> retC)
            m += (fun -> fun)
            newVaribleHelperFunction(d.subst(Substitution(b +: c, retC +: args)))(m)
          } else {
            tree 
          }
        case _ =>
          tree
      }

      (i + 1, fixedPoint(inlineT(tree)(State(census(tree))))(shrink))
    }

    trees.takeWhile{ case (_, tree) => size(tree) <= maxSize }.last._2
  }

  def newVaribleHelperFunction(body: Tree)(implicit m: Map[Symbol, Symbol]): Tree = body match {
    case LetL(name, value, body) =>
      val newName = Symbol.fresh("l")
      var nm = m
      if(!(m contains name)){
        nm = m + ((name, newName))
      }
      LetL(newName, value, newVaribleHelperFunction(body)(nm))

    case LetP(name, operation, args, body) if (m contains name) =>
      val newArgs = args map { arg => if(m contains arg) m.apply(arg) else arg }
      LetP(m.apply(name), operation, args, newVaribleHelperFunction(body))

    case LetP(name, operation, args, body) =>
      val name1 = Symbol.fresh("p")
      val nm = m + ((name, name1))
      // FUNCTIONS CAN GRAB EVERYTHING THEY WANT FROM ANYWEHRE WHAT?
      // A WHOLE NEW WORLD
      // BLESS THIS WORLS
      // HOW DOES ANYTHING WORK EVER WITH NWHAT
      val newArgs = args map { arg => if(m contains arg) m.apply(arg) else arg }
      LetP(name1, operation, newArgs, newVaribleHelperFunction(body)(nm))

    case If(cond, args, thenC, elseC) =>
      val newthenC = if(m contains thenC) m.apply(thenC) else thenC
      val newelseC = if(m contains elseC) m.apply(elseC) else elseC
      val newArgs = args map { arg => if(m contains arg) m.apply(arg) else arg }
      If(cond, newArgs, newthenC, newelseC)

    case LetC(cnts, body) =>
      val realNames = cnts map { case CntDef(name, _, _) => name }
      val newNames = realNames map { n => if(m contains n) m.apply(n) else Symbol.fresh("cnt") }
      // println("CNTS: " + realNames + "\n\n" + newNames)
      val ncnts = (newNames zip cnts) map { 
        case (cname1, CntDef(cname, cargs, boddyy)) =>
          val cargss = cargs map { arg => if(m contains arg) m.apply(arg) else Symbol.fresh("c") }
          var ns = m + ((cname, cname1))
          ns = ns ++ (cargs zip cargss)
          CntDef(cname1, cargss, newVaribleHelperFunction(boddyy)(ns))
      }
      val ns = m ++ (realNames zip newNames)
      LetC(ncnts, newVaribleHelperFunction(body)(ns))
    
    case LetF(funs, body) =>
      val fnames = funs map { case FunDef(name, _, _, _) => name }
      val fnames1 = fnames map {n => if(m contains n) m.apply(n) else Symbol.fresh("f") }
      val nfuns = (fnames1 zip funs) map {
        case (fname1, FunDef(fname, retC, fargs, boddyy)) =>
          val fargss = fargs map { arg => if(m contains arg) m.apply(arg) else Symbol.fresh("f") }
          val newretC = if(m contains retC) m.apply(retC) else Symbol.fresh("retc")
          val ns = m + ((fname, fname1)) + ((retC, newretC)) ++ (fargs zip fargss)
          FunDef(fname1, newretC, fargss, newVaribleHelperFunction(boddyy)(ns))
      }
      val ns = m ++ (fnames zip fnames1)
      val nbody = newVaribleHelperFunction(body)(ns)
      LetF(nfuns, nbody)

    case AppC(cnt, args) =>
      AppC(if(m contains cnt) m.apply(cnt) else cnt, args map { arg => if(m contains arg) m.apply(arg) else arg })
      
    case AppF(fun, retC, args) =>
      AppF(if(m contains fun) m.apply(fun) else fun, if(m contains retC) m.apply(retC) else retC, args map { arg => if(m contains arg) m.apply(arg) else arg })

    case Halt(name) => 
      Halt(if(m contains name) m.apply(name) else name)

    case _ => 
      body
  }

  // Census computation
  private def census(tree: Tree): Map[Name, Count] = {
    val census = MutableMap[Name, Count]()
    val rhs = MutableMap[Name, Tree]()

    def incAppUse(symbol: Name): Unit = {
      val currCount = census.getOrElse(symbol, Count())
      census(symbol) = currCount.copy(applied = currCount.applied + 1)
      rhs remove symbol foreach addToCensus
    }

    def incValUse(symbol: Name): Unit = {
      val currCount = census.getOrElse(symbol, Count())
      census(symbol) = currCount.copy(asValue = currCount.asValue + 1)
      rhs remove symbol foreach addToCensus
    }

    def addToCensus(tree: Tree): Unit = (tree: @unchecked) match {
      case LetL(_, _, body) =>
        addToCensus(body)
      case LetP(_, _, args, body) =>
        args foreach incValUse; addToCensus(body)
      case LetC(cnts, body) =>
        rhs ++= (cnts map { c => (c.name, c.body) }); addToCensus(body)
      case LetF(funs, body) =>
        rhs ++= (funs map { f => (f.name, f.body) }); addToCensus(body)
      case AppC(cnt, args) =>
        incAppUse(cnt); args foreach incValUse
      case AppF(fun, retC, args) =>
        incAppUse(fun); incValUse(retC); args foreach incValUse
      case If(_, args, thenC, elseC) =>
        args foreach incValUse; incValUse(thenC); incValUse(elseC)
      case Halt(arg) =>
        incValUse(arg)
    }

    addToCensus(tree)
    census.toMap
  }

  private def sameLen(formalArgs: Seq[Name], actualArgs: Seq[Name]): Boolean =
    formalArgs.length == actualArgs.length

  private def size(tree: Tree): Int = (tree: @unchecked) match {
    case LetL(_, _, body) => size(body) + 1
    case LetP(_, _, _, body) => size(body) + 1
    case LetC(cs, body) => (cs map { c => size(c.body) }).sum + size(body)
    case LetF(fs, body) => (fs map { f => size(f.body) }).sum + size(body)
    case AppC(_, _) | AppF(_, _, _) | If(_, _, _, _) | Halt(_) => 1
  }

  // Returns whether a ValuePrimitive has side-effects
  protected val impure: ValuePrimitive => Boolean
  // Returns whether different applications of a ValuePrimivite on the
  // same arguments may yield different results
  protected val unstable: ValuePrimitive => Boolean
  // Extracts the tag from a block allocation primitive
  protected val blockAllocTag: PartialFunction[ValuePrimitive, Literal]
  // Returns true for the block tag primitive
  protected val blockTag: ValuePrimitive
  // Returns true for the block length primitive
  protected val blockLength: ValuePrimitive
  // Returns true for the identity primitive
  protected val identity: ValuePrimitive

  // ValuePrimitives with their left-neutral elements
  protected val leftNeutral: Set[(Literal, ValuePrimitive)]
  // ValuePrimitives with their right-neutral elements
  protected val rightNeutral: Set[(ValuePrimitive, Literal)]
  // ValuePrimitives with their left-absorbing elements
  protected val leftAbsorbing: Set[(Literal, ValuePrimitive)]
  // ValuePrimitives with their right-absorbing elements
  protected val rightAbsorbing: Set[(ValuePrimitive, Literal)]
  // ValuePrimitives with the value equal arguments reduce to
  protected val sameArgReduce: PartialFunction[ValuePrimitive, Literal]
  // TestPrimitives with the (boolean) value equal arguments reduce to
  protected val sameArgReduceC: TestPrimitive => Boolean
  // An evaluator for ValuePrimitives
  protected val vEvaluator: PartialFunction[(ValuePrimitive, Seq[Literal]),
                                            Literal]
  // An evaluator for TestPrimitives
  protected val cEvaluator: PartialFunction[(TestPrimitive, Seq[Literal]),
                                            Boolean]
}

object CPSOptimizerHigh extends CPSOptimizer(SymbolicCPSTreeModule)
    with (SymbolicCPSTreeModule.Tree => SymbolicCPSTreeModule.Tree) {
  import treeModule._

  protected val impure: ValuePrimitive => Boolean =
    Set(MiniScalaBlockSet, MiniScalaByteRead, MiniScalaByteWrite)

  protected val unstable: ValuePrimitive => Boolean = {
    case MiniScalaBlockAlloc(_) | MiniScalaBlockGet | MiniScalaByteRead => true
    case _ => false
  }

  protected val blockAllocTag: PartialFunction[ValuePrimitive, Literal] = {
    case MiniScalaBlockAlloc(tag) => IntLit(tag)
  }
  protected val blockTag: ValuePrimitive = MiniScalaBlockTag
  protected val blockLength: ValuePrimitive = MiniScalaBlockLength

  protected val identity: ValuePrimitive = MiniScalaId

  protected val leftNeutral: Set[(Literal, ValuePrimitive)] =
    Set((IntLit(0), MiniScalaIntAdd), 
    (IntLit(1), MiniScalaIntMul), 
    (IntLit(~0), MiniScalaIntBitwiseAnd), 
    (IntLit(0), MiniScalaIntBitwiseOr), 
    (IntLit(0), MiniScalaIntBitwiseXOr))

  protected val rightNeutral: Set[(ValuePrimitive, Literal)] =
    Set((MiniScalaIntAdd, IntLit(0)), 
    (MiniScalaIntSub, IntLit(0)), 
    (MiniScalaIntMul, IntLit(1)), 
    (MiniScalaIntDiv, IntLit(1)), 
    (MiniScalaIntArithShiftLeft, IntLit(0)), 
    (MiniScalaIntArithShiftRight, IntLit(0)), 
    (MiniScalaIntBitwiseAnd, IntLit(~0)), 
    (MiniScalaIntBitwiseOr, IntLit(0)), 
    (MiniScalaIntBitwiseXOr, IntLit(0)))

  protected val leftAbsorbing: Set[(Literal, ValuePrimitive)] =
    Set((IntLit(0), MiniScalaIntMul), 
    (IntLit(0), MiniScalaIntBitwiseAnd), 
    (IntLit(~0), MiniScalaIntBitwiseOr))

  protected val rightAbsorbing: Set[(ValuePrimitive, Literal)] =
    Set((MiniScalaIntMul, IntLit(0)), 
    (MiniScalaIntBitwiseAnd, IntLit(0)), 
    (MiniScalaIntBitwiseOr, IntLit(~0)))

  protected val sameArgReduce: PartialFunction[ValuePrimitive, Literal] =
    Map(MiniScalaIntSub -> IntLit(0), 
    MiniScalaIntDiv -> IntLit(1), 
    MiniScalaIntMod -> IntLit(0), 
    MiniScalaIntBitwiseOr -> IntLit(~0))

  protected val sameArgReduceC: PartialFunction[TestPrimitive, Boolean] = {
    case MiniScalaIntLe | MiniScalaIntGe | MiniScalaEq => true
    case MiniScalaIntLt | MiniScalaIntGt | MiniScalaNe => false
  }

  protected val vEvaluator: PartialFunction[(ValuePrimitive, Seq[Literal]),
                                            Literal] = {
    case (MiniScalaIntAdd, Seq(IntLit(x), IntLit(y))) => IntLit(x + y)
    case (MiniScalaIntSub, Seq(IntLit(x), IntLit(y))) => IntLit(x - y)
    case (MiniScalaIntMul, Seq(IntLit(x), IntLit(y))) => IntLit(x * y)
    case (MiniScalaIntDiv, Seq(IntLit(x), IntLit(y))) if (y != 0) => (IntLit(Math.floorDiv(x, y)))
    case (MiniScalaIntMod, Seq(IntLit(x), IntLit(y))) if (y != 0) => (IntLit(Math.floorMod(x, y)))
    case (MiniScalaIntArithShiftLeft, Seq(IntLit(x), IntLit(y))) => (IntLit(x << y))
    case (MiniScalaIntArithShiftRight, Seq(IntLit(x), IntLit(y))) => (IntLit(x >> y))
    case (MiniScalaIntBitwiseAnd, Seq(IntLit(x), IntLit(y))) => (IntLit(x & y))
    case (MiniScalaIntBitwiseOr, Seq(IntLit(x), IntLit(y))) => (IntLit(x | y))
    case (MiniScalaIntBitwiseXOr, Seq(IntLit(x), IntLit(y))) => (IntLit(x ^ y)) 
    case (MiniScalaIntToChar, Seq(IntLit(x))) => CharLit(x.toChar)
    case (MiniScalaCharToInt, Seq(CharLit(x))) => IntLit(x)
    case (MiniScalaIntSub, Seq(IntLit(x))) => IntLit(-x)
    case (MiniScalaIntAdd, Seq(IntLit(x))) => IntLit(+x)
    case (MiniScalaBlockTag, Seq(v)) => v
  }

  protected val cEvaluator: PartialFunction[(TestPrimitive, Seq[Literal]),
                                            Boolean] = {

    case (MiniScalaIntP, Seq(IntLit(_))) => true
    case (MiniScalaBoolP, Seq(BooleanLit(_))) => true
    case (MiniScalaCharP, Seq(CharLit(_)))    => true
    case (MiniScalaUnitP, Seq(UnitLit))       => true
    case (MiniScalaIntLt, Seq(IntLit(x),IntLit(y))) => x < y
    case (MiniScalaIntLe, Seq(IntLit(x),IntLit(y))) => x <= y
    case (MiniScalaEq, Seq(IntLit(x),IntLit(y))) => x == y
    case (MiniScalaNe, Seq(IntLit(x),IntLit(y))) => x != y
    case (MiniScalaEq, Seq(BooleanLit(x),BooleanLit(y))) => x == y
    case (MiniScalaNe, Seq(BooleanLit(x),BooleanLit(y))) => x != y
    case (MiniScalaIntGe, Seq(IntLit(x),IntLit(y))) => x >= y
    case (MiniScalaIntGt, Seq(IntLit(x),IntLit(y))) => x > y
    // case _ => false // could also use this i suppose
    case (MiniScalaIntP,  _)       => false
    case (MiniScalaBoolP, _)       => false
    case (MiniScalaCharP, _)       => false
    case (MiniScalaUnitP, _)       => false
    case (MiniScalaBlockP, _)      => false
  }
}

object CPSOptimizerLow extends CPSOptimizer(SymbolicCPSTreeModuleLow)
    with (SymbolicCPSTreeModuleLow.Tree => SymbolicCPSTreeModuleLow.Tree) {
  import treeModule._

  protected val impure: ValuePrimitive => Boolean =
    Set(CPSBlockSet, CPSByteRead, CPSByteWrite)

  protected val unstable: ValuePrimitive => Boolean = {
    case CPSBlockAlloc(_) | CPSBlockGet | CPSByteRead => true
    case _ => false
  }

  protected val blockAllocTag: PartialFunction[ValuePrimitive, Literal] = {
    case CPSBlockAlloc(tag) => tag
  }
  protected val blockTag: ValuePrimitive = CPSBlockTag
  protected val blockLength: ValuePrimitive = CPSBlockLength

  protected val identity: ValuePrimitive = CPSId

  protected val leftNeutral: Set[(Literal, ValuePrimitive)] =
    Set((0, CPSAdd), (1, CPSMul), (~0, CPSAnd), (0, CPSOr), (0, CPSXOr))
  protected val rightNeutral: Set[(ValuePrimitive, Literal)] =
    Set((CPSAdd, 0), 
    (CPSSub, 0), 
    (CPSMul, 1), 
    (CPSDiv, 1),
    (CPSArithShiftL, 0), 
    (CPSArithShiftR, 0),
    (CPSAnd, ~0), 
    (CPSOr, 0), 
    (CPSXOr, 0))

  protected val leftAbsorbing: Set[(Literal, ValuePrimitive)] =
    Set((0, CPSMul), (0, CPSAnd), (~0, CPSOr))
  protected val rightAbsorbing: Set[(ValuePrimitive, Literal)] =
    Set((CPSMul, 0), (CPSAnd, 0), (CPSOr, ~0))

  protected val sameArgReduce: Map[ValuePrimitive, Literal] =
    Map(CPSSub -> 0, CPSDiv -> 1, CPSMod -> 0, CPSXOr -> 0)

  protected val sameArgReduceC: PartialFunction[TestPrimitive, Boolean] = {
    case CPSLe | CPSGe | CPSEq => true
    case CPSLt | CPSGt | CPSNe => false
  }

  protected val vEvaluator: PartialFunction[(ValuePrimitive, Seq[Literal]),
                                            Literal] = {
    case (CPSAdd, Seq(x, y)) => x + y
    case (CPSSub, Seq(x, y)) => x - y
    case (CPSMul, Seq(x, y)) => x * y
    case (CPSDiv, Seq(x, y)) if (y != 0) => Math.floorDiv(x, y)
    case (CPSMod, Seq(x, y)) if (y != 0) => Math.floorMod(x, y)

    case (CPSArithShiftL, Seq(x, y)) => x << y
    case (CPSArithShiftR, Seq(x, y)) => x >> y
    case (CPSAnd, Seq(x, y)) => x & y
    case (CPSOr, Seq(x, y)) => x | y
    case (CPSXOr, Seq(x, y)) => x ^ y
  }

  protected val cEvaluator: PartialFunction[(TestPrimitive, Seq[Literal]),
                                            Boolean] = {

    case (CPSLt, Seq(x, y)) => x < y
    case (CPSLe, Seq(x, y)) => x <= y
    case (CPSEq, Seq(x, y)) => x == y
    case (CPSNe, Seq(x, y)) => x != y
    case (CPSGe, Seq(x, y)) => x >= y
    case (CPSGt, Seq(x, y)) => x > y
  }
}
