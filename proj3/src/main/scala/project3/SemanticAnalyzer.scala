package project3

class SemanticAnalyzer(parser: Parser) extends Reporter with BugReporter {
  import Language._

  /**
   * Primitive functions that do not need to be defined or declared.
   */
  val primitives = Map[String,(Boolean,Type)](
      "getchar" -> (false, FunType(List(), IntType)),
      "putchar" -> (false, FunType(List(("", IntType)), UnitType))
    )

  /*
   * Define an empty state for the Semantic Analyzer.
   *
   * NOTE:
   *   val env = new Env
   *
   *   env("hello") is equivalent to env.apply("hello")
   */
  class Env {
    def apply(name: String): Option[Type] = None
    def isVar(name: String) = false
  }

  /**
   * Env that keeps track of variables defined.
   * The map stores true if the variable is mutable,
   * false otherwise and its type.
   */
  case class TypeEnv(
    vars: Map[String,(Boolean, Type)] = primitives,
    outer: Env = new Env) extends Env {

    /**
     * Return true if the variable is already defined
     * in this scope
     */
    def isDefined(name: String) = vars.contains(name)

    /**
     * Make a copy of this object and add a mutable variable 'name'
     */
    def withVar(name: String, tp: Type): TypeEnv = {
      copy(vars = vars + (name -> (true, tp)))
    }

    /*
     * Make a copy of this object and add an immutable variable 'name'
     */
    def withVal(name: String, tp: Type): TypeEnv = {
      copy(vars = vars + (name -> (false, tp)))
    }

    /*
     * Make a copy of this object and add in the list of immutable variables.
     */
    def withVals(list: List[(String,Type)]): TypeEnv = {
      copy(vars = vars ++ (list map { t => (t._1, (false, t._2)) }).toMap)
    }

    /*
     * Return true if 'name' is a mutable variable defined in this scope
     * or in the outer scope.
     */
    override def isVar(name: String) = vars.get(name) match {
      case None => outer.isVar(name)
      case Some((mut, _)) => mut
    }

    /*
     * Return the Type if the variable 'name' is an option.
     * i.e. Some(tp) if the variable exists or None if it doesn't
     */
    override def apply(name: String): Option[Type] = vars.get(name) match {
      case Some((_, tp)) => Some(tp)
      case None => outer(name)
    }
  }

  // Error reporting
  var numError = 0
  def error(msg: String, pos: Position): Unit = {
    numError += 1
    parser.error(msg, pos)
  }

  // Warning reporting
  var numWarning = 0
  def warn(msg: String, pos: Position): Unit = {
    numWarning += 1
    parser.warn(msg, pos)
  }

  /*
   * Return a fresh name if a new variable needs to be defined
   */
  var next = 0
  def freshName(pref: String = "x") = {
    next += 1
    s"${pref}_$next"
  }

  /*
   * Auxiliary functions. May be useful.
   */
  def getName(arg: Any): String = arg match {
    case Arg(name, _, _) => name
    case FunDef(name, _, _, _) =>  name
    case _ => BUG(s"Don't know how to extract name from $arg")
  }

  def getPos(arg: Any): Position = arg match {
    case Arg(_, _, pos) => pos
    case fd@FunDef(_, _, _, _) => fd.pos
    case _ => BUG(s"Don't know how to extract position from $arg")
  }

  def checkDuplicateNames(args: List[Any]): Boolean = args match {
    case h::t =>
      val name = getName(h)
      val (dup, other) = t partition { arg => name == getName(arg) }
      dup foreach { arg =>
        error(s"$name is already defined", getPos(arg))
      }
      checkDuplicateNames(other) || dup.length > 0
    case Nil => false
  }

  def funType(args: List[Arg], rtp: Type): FunType = {
    FunType(args map { arg => (arg.name, arg.tp) }, rtp)
  }

  def listArgType(size: Int, tp: Type) = List.fill(size)(("", tp))

  /**
   * Run the Semantic Analyzer on the given AST.
   *
   * Print out the number of warnings and errors found, if any.
   * Return the AST with types resolved and the number of warnings
   * and errors.
   *
   * NOTE: we want our main program to return an Int!
   */
  def run(exp: Exp) = {
    numError = 0
    val nexp = typeCheck(exp, IntType)(TypeEnv())
    if (numWarning > 0)
      System.err.println(s"""$numWarning warning${if (numWarning != 1) "s" else ""} found""")
    if (numError > 0)
      System.err.println(s"""$numError error${if (numError != 1) "s" else ""} found""")

    (nexp, numWarning, numError)
  }

  // List of valid infix operators
  val isBOperator   = Set("==","!=","<=",">=","<",">")
  val isIntOperator   = Set("+","-","*","/")

  /**
   * Returns the type of the binary operator 'op'. See case "+" for an example
   * TODO: implement the remaining binary operators for typeBinOperator
   */
  def typeBinOperator(op: String)(pos: Position) = op match {
    case "+" => FunType(List(("", IntType), ("", IntType)), IntType)
    case "-" => FunType(List(("", IntType), ("", IntType)), IntType)
    case "*" => FunType(List(("", IntType), ("", IntType)), IntType)
    case "/" => FunType(List(("", IntType), ("", IntType)), IntType)
    case "==" | "!=" | "<=" | ">=" | "<" | ">" => 
      FunType(List(("", IntType), ("", IntType)), BooleanType)
    case _ =>
      error("undefined binary operator", pos)
      UnknownType
  }

  // List of valid unary operators
  val isIntUnOperator   = Set("+","-")

  /**
   * Returns the type of the unary operator 'op'
   * TODO: implement typeUnOperator
   */
  def typeUnOperator(op: String)(pos: Position) = op match {
    case "+"  => // only ints can be postive / negative i think
      FunType(List(("", IntType)), IntType)
    case "-" =>
      FunType(List(("", IntType)), IntType)
    case _ =>
      error(s"undefined unary operator", pos)
      UnknownType
  }

  /**
   * Returns the type of the ternary operator 'op'
   * TODO: implement typeTerOperator
   * operators: block-set
   */
  def typeTerOperator(op: String)(pos: Position) = op match {
    case "block-set" => 
      // UnitType
      FunType(List(("", ArrayType(IntType)), ("", IntType)), IntType)
    case _ =>
      error(s"undefined ternary operator", pos)
      UnknownType
  }
  /*
   * Return the type of the operator 'op' with arity 'arity'
   */
  def typeOperator(op: String, arity: Int)(pos: Position): Type = arity match {
    case 3 => typeTerOperator(op)(pos)
    case 2 => typeBinOperator(op)(pos)
    case 1 => typeUnOperator(op)(pos)
    case _ =>
      error(s"undefined operator", pos)
      UnknownType
  }

  /**
   * Check if 'tp' conforms to 'pt' and return the more precise type.
   * The result needs to be well formed.
   *
   * TODO: implement the case of function type.
   */
  def typeConforms(tp: Type, pt: Type)(env: TypeEnv, pos: Position): Type = (tp, pt) match {
    case (_, _) if tp == pt => typeWellFormed(tp)(env, pos)
    case (_, UnknownType) => typeWellFormed(tp)(env, pos)  // tp <: Any
    case (UnknownType, _) => typeWellFormed(pt)(env, pos)  // for function arguments
    case (FunType(args1, rtp1), FunType(args2, rtp2)) if args1.length == args2.length =>
      args1.zip(args2).map { case (a,b) => typeConforms(a._2, b._2)(env, pos) }
      // FunType(args1,rtp1);
      FunType(args1,typeConforms(rtp1, rtp2)(env, pos));
    case (ArrayType(tp), ArrayType(pt)) => ArrayType(typeConforms(tp, pt)(env, pos))
    case _ => error(s"type mismatch;\nfound   : $tp\nexpected: $pt", pos); pt
  }

  /*
   * Auxiliary function used to check function type argument conformity.
   *
   * The function is verifying that 'tp' elements number n conforms
   * to 'pt' element number n. It returns the list of precise types
   * returned by each invocation to typeConforms
   */
  def typeConform(tp: List[(String, Type)], pt: List[(String,Type)])(env: TypeEnv, pos: Position): List[(String, Type)] = {
    if (tp.length != pt.length) BUG("length of list does not match")

    (tp zip pt) map { case ((arg1, tp1), (arg2, tp2)) =>
      (if (tp1 != UnknownType) arg1
       else arg2, typeConforms(tp1, tp2)(env, pos))
    }
  }

  /*
   * Verify that the type 'tp' is well formed. i.e there is no
   * UnknownType.
   */
  def typeWellFormed(tp: Type)(env: TypeEnv, pos: Position)(implicit forFunction: Boolean=false): Type = tp match {
    case FunType(args, rte) =>
      FunType(args map { case (n, tp) =>
        (n, typeWellFormed(tp)(env, pos))
      }, typeWellFormed(rte)(env, pos)(true))
    case ArrayType(tp) => ArrayType(typeWellFormed(tp)(env, pos))
    case UnknownType =>
        if (forFunction) error("malformed type: function return types must be explicit if function is used recursively or in other functions' bodies", pos)
        else error("malformed type", pos)
        UnknownType
    case _ => tp
  }


  /**
   * typeCheck takes an expression and an expected type (which may be UnknownType).
   * This is done via calling the typeInfer and typeConforms
   * functions (details below), and finally returning the original
   * expression with all typing information resolved.
   *
   * typeInfer uses the inference rules seen during the lectures
   * to discover the type of an expression. As a reminder, the rules we saw can be
   * found in lectures 5 and 6.
   *
   * TODO: Remove the ??? and add the correct implementation.
   * The code must follow the inference rules seen during the lectures.
   *
   * The errors/warnings check that you had to implement for project 2
   * should be already implemented. However, there are new variables
   * introduced that need to be check for duplicate (function name,
   * variables names). We defined the rules for function semantic in
   * lecture 5.
   */
  def typeCheck(exp: Exp, pt: Type)(env: TypeEnv): Exp = {
    val nexp = typeInfer(exp, pt)(env)
    val rnexpType = typeConforms(nexp.tp, pt)(env, exp.pos)
    nexp.withType(rnexpType)
  }

  def typeInfer(exp: Exp, pt: Type)(env: TypeEnv): Exp = exp match {
    case Lit(_: Int) => 
      exp.withType(IntType)
    case Lit(_: Boolean) => 
      exp.withType(BooleanType)
    case Lit(_: Unit) => 
      exp.withType(UnitType)
    case Prim("block-set", args) => 
      // List(Ref(arr), Lit(0), Lit(1))
      println("Args PRIM1: " + args)
      // println("op PRIM1: " + op)
      val typev = typeCheck(args(2), UnknownType)(env);
      println("typee: " + typev.tp)
      val typei = typeCheck(args(1), IntType)(env);

      val typearr = typeCheck(args(0), ArrayType(typev.tp))(env);

      val list = List(typearr, typei, typev);

      val x = Prim("block-set", list).withType(UnitType);
      println("XXXXXXX: " + typev);
      x
    case Prim(op, args) =>
      println("Args PRIM2: " + args)
      println("op PRIM2: " + op)
      typeOperator(op, args.length)(exp.pos) match {
        case FunType(atps, rtp) => 
          // println("atps: " + atps);
          // exp.withType(rtp)
          val tc1 = typeCheck(args(0), atps(0)._2)(env);
          val tc2 = typeCheck(args(1), atps(1)._2)(env);
          val fakeArgs = List(tc1, tc2);
          // println("fakeArgs: " + tc1.tp)
          // println("fakeArgs: " + tc2.tp)
          // typeCheck(tc1, tc2.tp)(env);
          // typeCheck(tc1, rtp)(env);
          // typeCheck(tc1, rtp)(env);
          if(tc1.tp != tc2.tp) {
            error("Types are not the same");
          }
          Prim(op, fakeArgs).withType(rtp);
        case _ => BUG("operator's type needs to be FunType")
      }
    case Let(x, tp, rhs, body) =>
      if (env.isDefined(x))
        warn("reuse of variable name", exp.pos)
      val nrhs = typeCheck(rhs, tp)(env)
      val nbody = typeCheck(body, pt)(env.withVal(x, nrhs.tp))
      Let(x, nrhs.tp, nrhs, nbody).withType(nbody.tp)
    case Ref(x) =>
      env(x) match {
        case Some(tp) => exp.withType(tp)
        case _ =>
          error("undefined identifier", exp.pos)
          exp.withType(UnknownType)
      }
    case If(cond, tBranch, eBranch) =>
      // Hint: type check the else branch before the then branch.
      val ccond = typeCheck(cond, BooleanType)(env)
      val eeBranch = typeCheck(eBranch, pt)(env)
      val ttBranch = typeCheck(tBranch, eBranch.tp)(env)
      If(ccond, ttBranch, eeBranch).withType(eBranch.tp)
    case VarDec(x, tp, rhs, body) =>
      if (env.isDefined(x)){
        warn("reuse of variable name", exp.pos)
      }
      val rrhs = typeCheck(rhs, tp)(env)
      val bbody = typeCheck(body, pt)(env.withVar(x, rrhs.tp))
      VarDec(x, rrhs.tp, rrhs, bbody).withType(bbody.tp)
    case VarAssign(x, rhs) =>
      val xtp = if (!env.isDefined(x)) {
        error("undefined identifier", exp.pos)
        UnknownType
      } else {
        if (!env.isVar(x))
          error("reassignment to val", exp.pos)
        env(x).get
      }

      /* Because of syntactic sugar, a variable assignment
       * statement can be accepted as an expression
       * of type Unit. In this case, we will modify
       * the AST and store the assignment value into
       * a "dummy" variable and return the Unit Literal.
       *
       * For example,
       *
       * If(..., VarAssign("x", Lit(1)), Lit(()))
       *
       * requires the two branches of the If to be of the same
       * type, in this case, Unit. Therefore the "then" branch
       * will need to be modified to have the correct type.
       * Without changing the semantics!
       */
      //  case Let(x, tp, rhs, body)
      val rrhs = typeCheck(rhs, xtp)(env)
      // val rrhs = typeCheck(rhs, env(x) match {
      //   case Some(tp) => tp
      //   case _ => UnknownType })(env)
      // println("pt: " + pt)
      // println("rrhs.tp: " + rrhs.tp)
      pt match {
        case BooleanType => 
          // println("1???")
          VarAssign(x, rrhs).withType(rrhs.tp) 
        case IntType =>
          // println("2???")
          VarAssign(x, rrhs).withType(rrhs.tp) 
        case UnitType => 
          // println("3???")
          // VarAssign(x, rrhs).withType(rrhs.tp) 
          Let(freshName(), pt, VarAssign(x, rrhs), Lit(()) ).withType(pt)
        case _ => 
          // println("4???")
          // exp.withType(UnknownType)
          VarAssign(x, rrhs).withType(rrhs.tp) 
      }
    case While(cond, lbody, body) => 
      val ccond = typeCheck(cond, BooleanType)(env)
      val llbody = typeCheck(lbody, UnitType)(env)
      val bbody = typeCheck(body, pt)(env)
      While(ccond, llbody, bbody).withType(bbody.tp)
    case FunDef(fname, args, rtp, fbody) => 
      // println(args);
      // println("AHHHH")
      // println("rtp: " + rtp)
      checkDuplicateNames(args)
      // println("AHHHH0")
      // var tp = rtp match {
      //   case FunType(_, rtp) => rtp
      // }
      // println("AHHHH1")
      val argsList = args map {case Arg(name, tp, _) => (name, tp)}
      // println("argsList: " + argsList)
      // println("fbody: " + fbody)
      val nfbody = typeCheck(fbody, rtp)(env.withVals(argsList))
      // println("nfbody: " + nfbody)
      val nrtp = FunType(argsList, nfbody.tp); 
      // val nrtp = rtp match {
      //   case FunType(args, _) => FunType(args, nfbody.tp)
      // }
      // println("AHHHH4")

      FunDef(fname, args, nfbody.tp, nfbody).withType(nrtp)
      // println("nrtp af÷ter to return " + fff.tp)
      // fff
    case LetRec(funs, body) => // funs = is the list of functions
      checkDuplicateNames(funs);
      // println("funs: " + funs)
      // println("body: " + body)
      // // TODO modify to handle general case
      // for(i <- 0 to funs.length-1){
      //   val argsList = args map {case Arg(, tp, _) => (name, rtp)}
      //   val nfenv = env.withVals();
      //   val nbody = typeCheck()()
      // }
      
      // println(argsList);



      var e = env;
      for(i <- 0 to funs.length-1) {
        val argsList = funs(i).asInstanceOf[FunDef].args map {case Arg(name, tp, _) => (name, tp)}
        e = e.withVal(funs(i).asInstanceOf[FunDef].name, FunType(argsList, funs(i).asInstanceOf[FunDef].rtp)); 
      }

      // println("NEW ENV " + e)
      var eTwo = e;
      val nfuns = funs.map {case f@FunDef(_, _, _, _) => 
                            val argsList = f.asInstanceOf[FunDef].args map {case Arg(name, tp, _) => (name, tp)}
                            val y = FunType(argsList, f.asInstanceOf[FunDef].rtp)
                            println("before i call")
                            var gg = typeCheck(f, y)(e);
                            println("after i call " + gg.tp)
                            eTwo = eTwo.withVal(f.name, gg.tp);
                            gg
                            }
      val nbody = typeCheck(body, pt)(eTwo)
      // println("nbody " + nbody);
      LetRec(nfuns, nbody).withType(nbody.tp)

      
      
      // // println("AHHHH2")
      // val nfbody = typeCheck(fbody, rtp)(env.withVals(argsList))
      // // println("AHHHH3")
      // val nrtp = FunType(argsList, nfbody.tp); 
      // // val nrtp = rtp match {
      // //   case FunType(args, _) => FunType(args, nfbody.tp)
      // // }
      // // println("AHHHH4")
      // println("NRTP " + nrtp)
      // val fff = FunDef(fname, args, rtp, nfbody).withType(nrtp)
      // println("fff " + fff)





      // val fenv = env.withVals(funs.map {case f@FunDef(name, _, rtp, _) => (name, f.tp)})
      // println("fenv " + fenv);
      // // println("rtp " + rtp);
      // // FunType(args, rtp)
      // val nfuns = funs.map {case f@FunDef(_, _, _, _) => 
      //                             println("f: " + f);
      //                             println("f.tp: " + f.tp);
      //                             typeCheck(f, f.tp)(fenv) }
      // println("nfuns " + nfuns);
      // val nfenv = env.withVals(nfuns.map {case f@FunDef(name, _, _, _) => (name, f.tp)})
      // println("nfenv " + nfenv);
      // val nbody = typeCheck(body, pt)(nfenv)
      // println("nbody " + nbody);
      // LetRec(nfuns, nbody).withType(nbody.tp)
    case App(fun, args) =>
      println("fun: " + fun)
      println("args: " + args)
      // val nFun: Exp = fun match {
      //   case Ref(x) =>
      //     var res = env(x) match {
      //       case Some(tp) => 
      //         fun.withType(tp)
      //       case _ => 
      //         println("I GOTHERE BEST FRIEND")
      //         fun.withType(UnknownType)
      //     }
      //     res
      //   case _ => fun.withType(UnknownType) // Maybe I dont need. 
      // }
      val nFun: Exp = typeCheck(fun, fun.tp)(env)
      // val nFun : Exp = typeCheck(fun, pt)(env)

      // Handling some errors
      val ftp = nFun.tp match {
        case ftp@FunType(fargs, _) if fargs.length == args.length =>
          ftp
        case ftp@FunType(fargs, rtp) if fargs.length < args.length =>
          error(s"too many arguments for method: ($fargs)$rtp", exp.pos)
          FunType(fargs ++ List.fill(args.length - fargs.length)(("", UnknownType)), rtp)
        case ftp@FunType(fargs, rtp) =>
          error(s"not enough arguments for method: ($fargs)$rtp", exp.pos)
          ftp
        case ArrayType(tp) =>
          FunType(List(("", IntType)), tp)
        case tp =>
          error(s"$tp does not take paramters", exp.pos)
          FunType(List.fill(args.length)(("", UnknownType)), pt)
      }

      // TODO: Check arguments type
      println("FTP: " + ftp)
      var nargs: List[Exp] = List[Exp]()
      for( i <- 0 to args.length - 1){
        val nArg = typeCheck(args(i), ftp.args(i)._2)(env);
        nargs = nArg +: nargs
      }
      nargs = nargs.reverse;
      println("nargs: " + nargs)


      // Transform some function applications into primitives on arrays.
      nFun.tp match {
        case ArrayType(tp) =>
          Prim("block-get", List(nFun, nargs.head)).withType(tp)
        case _ => 
          println("EAWFJAWGJAFGEWAWEFAEWFAFEW")
          App(nFun, nargs).withType(ftp.rtp)
      }
    case ArrayDec(size: Exp, etp: Type) =>
      // TODO: Check array declaration
      // Note that etp is the type of elements
      val ssize = typeCheck(size, IntType)(env)
      ArrayDec(ssize, etp).withType(etp)
    case _ => BUG(s"malformed expresstion $exp")
  }
}
