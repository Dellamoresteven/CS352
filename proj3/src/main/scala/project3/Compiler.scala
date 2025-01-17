package project3
abstract class X86Compiler extends BugReporter with Codegen {
  import Language._
  /*
   * Abstract class used to store the location
   */
  abstract class Loc {
    def +(y: Int): Loc
  }
  /*
   * Register location, the physical location
   * can be addressed with the register #sp
   */
  case class Reg(sp: Int) extends Loc {
    def +(y: Int) = Reg(sp+y)
  }
  /*
   * Function location, the physical location
   * can be addressed directly with the name
   */
  case class Func(name: String) extends Loc {
    def +(y: Int) = BUG("This Loc should not be used as a stack location.")
  }
  // Function to extra physical address from Loc
  // CHANGE: instead of using regs(...) directly
  // we now use the function loc.
  def loc(l: Loc): String = l match {
    case Reg(sp) => avRegs(sp)
    case Func(name) => name
  }
  def loc(sp: Int): String = avRegs(sp)
  // List of available register.
  // DO NOT CHANGE THE REGISTERS!!
  val avRegs = Seq("%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9", "%r10", "%r11", "%r12", "%r13", "%r14", "%r15")
  /****************************************************************************/
  def onMac = System.getProperty("os.name").toLowerCase contains "mac"
  val entry_point = "entry_point"
  def funcName(name: String) = (if (onMac) "_" else "") + name
  /**
   * Env of the compiler. Keep track of the location
   * in memory of each variable defined.
   */
  val primitives = Map(
    "putchar" -> Func("putchar"),
    "getchar" -> Func("getchar"))
  private class Env {
    def undef(name: String) = BUG(s"Undefined identifier $name (should have been found during the semantic analysis)")
    def apply(name: String): Loc =  undef(name)
  }
  private case class LocationEnv(
    vars: Map[String, Loc] = primitives,
    outer: Env = new Env) extends Env {
      /*
       * Return a copy of the current state plus a
       * variable 'name' at the location 'loc'
       */
      def withVal(name: String, loc: Loc): LocationEnv = {
        copy(vars = vars + (name -> loc))
      }
      /*
       * Return a copy of the current state plus all
       * variables in 'list'
       */
      def withVals(list: List[(String,Loc)]): LocationEnv = {
        copy(vars = vars ++ list.toMap)
      }
      /*
       * Return the location of the variable 'name'
       */
      override def apply(name: String): Loc = vars.get(name) match {
        case Some(loc) => loc
        case _  => outer(name)
      }
  }

  /*
   * Generate code that computes the unary operator
   * 'op' on the value at memory location 'sp' and that
   * stores the result at 'sp'.
   */
  def transUn(op: String)(sp: Loc) = op match {
    case "+" => () // nothing to do!
    case "-" => emitln(s"negq ${loc(sp)}")
    case _ => BUG(s"Unary operator $op undefined")
  }

  /*
   * Generate code that computes the binary operator
   * 'op' on the values at memory location 'sp' and
   * 'sp1' and that stores the result at 'sp'.
   *
   * TODO: implement missing operators.
   * Here are the valid operators:
   * +, -, *, /, ==, !=, <=, <, >=, >, block-get
   */
  def transBin(op: String)(sp: Loc, sp1: Loc) = op match {
    case "+" => 
      emitln(s"addq ${loc(sp1)}, ${loc(sp)}")
    case "-" => emitln(s"subq ${loc(sp1)}, ${loc(sp)}")
    case "*" => emitln(s"imul ${loc(sp1)}, ${loc(sp)}")
    case "/" =>
      emitln(s"movq ${loc(sp)}, %rax")
      emitln(s"pushq %rdx") // save $rdx for the division
      emitln(s"movq ${loc(sp1)}, %rbx") // in case sp1 == %rdx
      emitln(s"cqto")
      emitln(s"idiv %rbx")
      emitln(s"popq %rdx") // put back
      emitln(s"movq %rax, ${loc(sp)}")
    case "==" =>
      emitln(s"cmp ${loc(sp1)}, ${loc(sp)}")
      emitln(s"sete %al")
      emitln(s"movzbq %al, ${loc(sp)}")
    case "!=" =>
      emitln(s"cmp ${loc(sp1)}, ${loc(sp)}")
      emitln(s"setne %al")
      emitln(s"movzbq %al, ${loc(sp)}")
    case "<=" => 
      emitln(s"cmp ${loc(sp1)}, ${loc(sp)}")
      emitln(s"setle %al")
      emitln(s"movzbq %al, ${loc(sp)}")
    case "<" => 
      emitln(s"cmp ${loc(sp1)}, ${loc(sp)}")
      emitln(s"setl %al")
      emitln(s"movzbq %al, ${loc(sp)}")
    case ">=" =>
      emitln(s"cmp ${loc(sp1)}, ${loc(sp)}")
      emitln(s"setge %al")
      emitln(s"movzbq %al, ${loc(sp)}")
    case ">" =>       
      emitln(s"cmp ${loc(sp1)}, ${loc(sp)}")
      emitln(s"setg %al")
      emitln(s"movzbq %al, ${loc(sp)}")
    case "block-get" =>
      // println("block-get-1")
      // emitln(s"movq (${loc(sp)}, ${loc(sp1)}, 8), ${loc(sp)}")
      // // emitln(s"movq $$2, ${loc(sp)}")

      // println("${loc(sp)} -> " + loc(sp))
      // // emitln(s"movq (${loc(sp)}), ${loc(sp)}")
      
      emitln(s"movq (${loc(sp)}, ${loc(sp1)}, 8), ${loc(sp)}")
      // emitln(s"movq heap(%rip), ${loc(sp)}")
      
      // println("block-get-2")
    case _ => BUG(s"Binary operator $op undefined")
  }

  /*
   * Generate code that computes the ternary operator
   * 'op' on the values at memory location 'sp', 'sp1 and'
   * 'sp2' and that stores the result at 'sp'.
   *
   * TODO: implement the missing operator
   * Valid operators: block-set
   */
  def transTer(op: String)(sp: Loc, sp1: Loc, sp2: Loc) = op match {
    case "block-set" =>
      // println("block-set-1")
      // println("${loc(sp)} -> " + loc(sp))
      emitln(s"movq ${loc(sp2)}, (${loc(sp)}, ${loc(sp1)}, 8)")
      // emitln(s"movq ${loc(sp2)}, heap(%rip)")
      // emitln(s"movq  ${loc(sp2)}, (${loc(sp)}, ${loc(sp1)}, 8)")
      // emitln(s"leaq (${loc(sp)}, ${loc(sp1)}, 8), ${loc(sp)}")
      // emitln(s"movq  ${loc(sp2)}, ${loc(sp)}")


      // emitln(s"movq heap(%rip), ${loc(sp)}")
      // emitln(s"movq  ${loc(sp)}, (${loc(sp1)}, ${loc(sp2)}, 8)")
      // emitln(s"movq  ${loc(sp2)}, (${loc(sp1)}, ${loc(sp)}, 8)")
      // emitln(s"movq  ${loc(sp1)}, (${loc(sp)}, ${loc(sp2)}, 8)")
      // emitln(s"movq  ${loc(sp1)}, (${loc(sp2)}, ${loc(sp)}, 8)")
      // emitln(s"movq  ${loc(sp)}, (${loc(sp2)}, ${loc(sp1)}, 8)")
      // emitln(s"movq $$5, ${loc(sp)}")
      // println("block-set-2")
      // emitln(s"movq ${loc(sp2)}, heap(%rip)")
      // //       emitln(s"movq (${loc(sp)}, ${loc(sp1)}, 8), ${loc(sp)}")
      // // emitln(s"movq ${loc(sp2)}, ${loc(sp)}") //move sp2 into arr(i)

      // // emitln(s"movq (${loc(sp)}, ${loc(sp1)}, 8), %rip")


      // emitln(s"leaq heap(%rip), ${loc(sp2)}")



      // emitln(s"movq (${loc(sp)}, ${loc(sp1)}, 8), ${loc(sp)}")
      // emitln(s"movq ${loc(sp2)}, ${loc(sp)}") //move sp2 into arr(i)

      // emitln(s"movq (${loc(sp)}, ${loc(sp1)}, 8), %rip")
      // emitln(s"movq ${loc(sp2)}, heap(%rip)")
    case _ => BUG(s"ternary operator $op undefined")
  }

  def transPrim(op: String)(idxs: List[Loc]) = idxs match {
    case List(sp, sp1, sp2) => transTer(op)(sp, sp1, sp2)
    case List(sp, sp1)      => transBin(op)(sp, sp1)
    case List(sp)           => transUn(op)(sp)
    case _ => BUG(s"no prim with ${idxs.length} arguments")
  }

  type Label = String

  var nLabel = 0
  def freshLabel(pref: String) = { nLabel += 1; s"$pref$nLabel" }

  /*
   * Generate code that compute the result of the
   * computation represented by the AST 'exp'.
   */
  val global = (primitives.keySet + entry_point) map(funcName(_))
  // var funsLocG = map;
  def emitCode(exp: Exp): Unit = {
    emitln(".text", 0)
    emitln(s".global ${global mkString ", "}\n", 0)

    // Generate code for our AST
    trans(exp, Reg(0))(LocationEnv())

    emitln("#################### DATA #######################", 0)
    emitln("\n.data\nheap:\t.quad 0",0)
    emitln("#################################################", 0)
  }

  /*
   * Generate code that jump to the label 'label'
   * if the location 'sp' contains the value 'true'
   * TODO
   */   
  def transJumpIfTrue(sp: Loc)(label: Label) = {
    emitln(s"test ${loc(sp)}, ${loc(sp)}");
    emitln(s"jnz ${label}");
    // emitln(s"je ${label}");
  }

  /*
   * Generate code that compute the result og the
   * computation represented by the AST 'exp'. The
   * value will be placed at memory location 'sp'
   *
   * TODO: Fill in each TODO with the appropriate code.
   *
   * The ??? can be filled for extra credit.
   */
  def trans(exp: Exp, sp: Loc)(env: LocationEnv): Unit = exp match {
    case Lit(x: Int) =>
      emitln(s"movq $$$x, ${loc(sp)}")
    case Lit(b: Boolean) =>
      var bool = 1;
      b match {
        case true =>
          emitln(s"movq $$$bool, ${loc(sp)}")
        case false =>
          bool = 0;
          emitln(s"movq $$$bool, ${loc(sp)}")
      }
    case Lit(x: Unit) => () // TODO
    case Prim(op, args) =>
      val idxs = List.tabulate(args.length)(i => sp + i)
      (args zip idxs) foreach { case (arg, idx) => trans(arg, idx)(env) }
      transPrim(op)(idxs)
    case Let(x, tp, rhs, body) =>
      trans(rhs, sp)(env)
      if (tp == UnitType) { // simple optimization for Daniel
        trans(body, sp)(env)
      } else {
        trans(body, sp + 1)(env.withVal(x, sp))
        emitln(s"movq ${loc(sp + 1)}, ${loc(sp)}")
      }
    case Ref(x) =>
      env(x) match {
        case Reg(sp1) => emitln(s"movq ${loc(sp1)}, ${loc(sp)}")
        case Func(name) => name 
      }
    case If(cond, tBranch, eBranch) =>
      val lab = freshLabel("if")
      trans(cond, sp)(env)
      transJumpIfTrue(sp)(s"${lab}_then")
      trans(eBranch, sp)(env)
      emitln(s"jmp ${lab}_end")
      emitln(s"${lab}_then:", 0)
      trans(tBranch, sp)(env)
      emitln(s"${lab}_end:", 0)
    case VarDec(x, tp, rhs, body) =>
      trans(rhs, sp)(env)
      trans(body, sp + 1)(env.withVal(x, sp))
      emitln(s"movq ${loc(sp + 1)}, ${loc(sp)}")
    case VarAssign(x, rhs) =>
      trans(rhs, sp)(env)
      emitln(s"movq ${loc(sp)}, ${loc(env(x))}")
    case While(cond, lBody, body) =>
      val lab = freshLabel("loop")
      emitln(s"jmp ${lab}_cond")
      emitln(s"${lab}_body:", 0)
      trans(lBody, sp)(env)
      emitln(s"${lab}_cond:", 0)
      trans(cond, sp)(env)
      transJumpIfTrue(sp)(s"${lab}_body")
      trans(body, sp)(env)
    case LetRec(funs, body) =>
      emitln("################# FUNCTIONS #####################", 0)
      // We do not save the location of the function into register because we can use their
      // name as a label.
      val funsLoc = funs map { case FunDef(name, _, _, _) => (name, Func(name)) }
      // funsLocG = funsLoc;
      // println("EAWFAWEFAEWF: " + funsLoc)
      val fEnv = env.withVals(funsLoc)
      // TODO complete the code
      for(i <- 0 to funs.length-1) {
        val f = funs(i);
        
        // trans(f, sp+i+1)(fEnv);
        // println("AHHHH: " + Reg(0));
        // println("AHHHH: " + sp);
        trans(f, Reg(0))(fEnv);
      }

      emitln("#################################################\n\n", 0)
      emitln("###################### MAIN #####################", 0)
      //////////// DO NOT CHANGE////////////////
      emitln(s"${funcName(entry_point)}:", 0)
      emitln("pushq %rbp\t# save stack frame for calling convention")
      emitln("movq %rsp, %rbp")
      emitln("movq %rdi, heap(%rip)")
      //////////////////////////////////////////

      // emit the main function (body of LetRec) here
      // TODO you may need to change that code.
      trans(body, Reg(0))(env.withVals(funsLoc))
      emitln(s"movq ${loc(0)}, %rax")

      //////////// DO NOT CHANGE////////////////
      emitln("movq %rbp, %rsp\t# reset frame")
      emitln("popq %rbp")
      emitln("ret")
      emitln("#################################################\n\n", 0)
      //////////////////////////////////////////

    case FunDef(fname, args, _, fbody) =>
      //////////// DO NOT CHANGE////////////////
      emitln(s"${funcName(fname)}:", 0)
      emitln("pushq %rbp\t# save stack frame for calling convention")
      emitln("movq %rsp, %rbp")
      //////////////////////////////////////////

      // TODO
      val indexs = List.tabulate(args.length)(i => sp + i)
      // var indexs = List[Int]();
      // for( i <- 0 to args.length - 1){
      //   println("SP: " + sp)
      //   indexs = (sp+i) +: indexs
      // }
      //locations of all args
      val pos = (args zip indexs) map {case (Arg(name, _, _), index) => (name, index)}
      // println("pos: " + pos)
      //eval body
      trans(fbody, sp + args.length)(env.withVals(pos))
      emitln(s"movq ${loc(sp + args.length)}, %rax") // move to return

      //////////// DO NOT CHANGE////////////////
      emitln("movq %rbp, %rsp\t# reset frame")
      emitln("popq %rbp")
      emitln("ret\n")
      //////////////////////////////////////////
    case App(fun, args) =>
      // val indexs = List[Int](0);
      val indexs = List.tabulate(args.length)(i => sp + i)
      // val indexs = List.tabulate(args.length)(i => i % avRegs.length)

      var i = 0;
      // while(Reg(i) != sp) {
      //   emitln(s"push ${loc(i)}");
      //   i = i + 1;
      // }

      // (args zip indexs) map {case (arg, idx) => trans(arg, idx)(env)}
      val ff = (args zip indexs) map { case (arg, idx) => trans(arg, idx)(env) }
      // println("args: " + args)
      // println("indexs: " + indexs)
      // trans(args(0), Reg(0))(env)
      // println("LOL")
      // Compute the physical location of the function to be called
      val fLoc: String = fun match {
        case Ref(fname) =>
          env(fname) match {
            case Reg(sp) => 
              // println("sp " + sp); 
              // ??? //("call " + funcName(  )
              // ("jmp " + "*("+loc(sp)+")")
              ???
            case Func(name) => ("call " + funcName(name))
          }
        case _ => ??? // Extra credit
      }

      val spRegValue:Int = sp match {
        case Reg(x) => x
      }

      for(i <- 0 to avRegs.length-1){
        // println("i: " + i)
        emitln(s"push ${loc(i)}");
      }
      // println("spRegValue: " + spRegValue);
      // println("sp: " + sp);

      i = 0;
      while(i < args.length) {
        emitln(s"movq ${loc(sp + i)}, ${loc(i)}")
        i = i + 1;
      }

      emitln(s"${fLoc}")

      // i = 0
      // while(Reg(i) != sp) {
      //   emitln(s"pop ${loc(i)}")
      //   i = i + 1
      // }

      // i = spRegValue - 1;
      // while(i >= 0) {
      //   emitln(s"popq ${loc(i)}")
      //   i = i - 1;
      // }

      for(i <- avRegs.length-1 to 0 by -1){
        // println("i: " + i)
        emitln(s"pop ${loc(i)}");
      }

      emitln(s"movq %rax, ${loc(sp)}")

    case ArrayDec(size, _) =>
      // This node needs to allocate an area of eval(size) * 8 bytes in the heap
      // the assembly variable "heap" contains a pointer to the first valid byte
      // in the heap. Make sure to update its value accordingly.
      // TODO

      // println("I AM AN ARRAYYYYYY")
      // trans(size, sp+1)(env)
      // println("SIZEY " + size)
      // val num = 8
      // emitln(s"movq $$$num, %rax")
      // emitln(s"imulq ${loc(sp+1)}")
      // // trans(size, sp)(env);
      // // val steve = 8;
      // // emitln(s"imulq ${loc(sp)}, $$$steve");
      // emitln(s"movq heap(%rip), %rax");
      // emitln(s"movq heap(${loc(sp)}), %rip")



      // emitln(s"movq heap(%rip), ${loc(sp)}")

      // trans(size, sp + 1)(env) // offset

      // // emitln(s"movq heap(%rip), ${loc(sp + 1)}")
      // // emitln(s"movq heap(%rip), %rax")
      // emitln(s"movq (${loc(sp)}, ${loc(sp + 1)}, 8), ${loc(sp)}")
      // // emitln(s"movq ${loc(sp)}, heap(%rip)")
      // // emitln(s"movq heap(%rip), (${loc(sp)}, ${loc(sp + 1)}, 8)")

      // emitln(s"leaq heap(%rip), %rax");
      // // emitln(s"movq %rax, heap(%rip)");
      // // // emitln(s"movq %rax, (${loc(sp)}, ${loc(sp + 1)}, 8)")
      // // emitln(s"movq (%rax), %rax");
      // println("HERE0")

      emitln(s"movq heap(%rip), ${loc(sp)}")

      trans(size, sp + 1)(env)

      emitln(s"leaq (${loc(sp)}, ${loc(sp + 1)}, 8), ${loc(sp)}")

      emitln(s"movq ${loc(sp)}, heap(%rip)")
      // println("HERE1")

      
    case _ => BUG(s"don't know how to implement $exp")
  }
}


/*

HEAP -> SP
offset = (SP + 1)
SP + (SP+1) -> SP
SP -> HEAP

*/