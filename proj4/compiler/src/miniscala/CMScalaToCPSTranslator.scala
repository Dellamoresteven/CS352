package miniscala

import miniscala.{ SymbolicCMScalaTreeModule => S }
import miniscala.{ SymbolicCPSTreeModule => C }

object CMScalaToCPSTranslator extends (S.Tree => C.Tree) {
  def apply(tree: S.Tree): C.Tree = {
    nonTail(tree){ ret =>
      val z = Symbol.fresh("c0")
      // C.LetL(z, IntLit(0), C.Halt(ret)) // TODO REVERT TO C.Halt(z)
      C.LetL(z, IntLit(0), C.Halt(z))
    }(Set.empty)
  }

  private def nonTail(tree: S.Tree)(ctx: Symbol=>C.Tree)(implicit mut: Set[Symbol]): C.Tree = {
    // @unchecked to avoid bogus compiler warnings
    (tree: @unchecked) match {
      case S.Let(name, _, value, body) =>
        // println("AEWFEAFWFFFFW: \n\n\n\n\n\n");
        nonTail(value)(v =>
            C.LetP(name, MiniScalaId, Seq(v), nonTail(body)(ctx)))

      case S.Lit(value) =>
        // println("LIT LIT LIT\n");
        val freshSymbol = Symbol.fresh("n")
        C.LetL(freshSymbol, value, ctx(freshSymbol))

      // TODO: Dont tell me what to do.
      // case class LetF(funs: Seq[FunDef], body: Tree) extends Tree
      // case class FunDef(name: Name, ptps: List[String], args: List[Arg], rtp: Type, body: Tree)
      case S.LetRec(functions, body) =>
        // val funcDefs = functions map { f => 
        //     var fresh = Symbol.fresh("rc");
        //     C.FunDef(f.name, fresh ,f.args map { arg => arg.name}, nonTail(f.body)(v => C.AppC(fresh, Seq(v))))
        // }
        // C.LetF(funcDefs, nonTail(body)(ctx))

        /* TAIL */
        val funcDefs = functions map { f => 
            var fresh = Symbol.fresh("rc");
            C.FunDef(f.name, fresh, f.args map { arg => arg.name}, tail(f.body, fresh))
        }
        C.LetF(funcDefs, nonTail(body)(ctx))

      // Reference of an immutable variable
      case S.Ref(name) if !mut(name) =>
        // println("PLEASE GOD immutable " + name + "\n\n\n\n\n\n\n")
        ctx(name);

      // Reference of an mutable variable
      case S.Ref(name) if mut(name) =>
        // println("PLEASE GOD mutable " + name + "\n\n\n\n\n\n\n")
        // println("tree " + tree + "\n\n\n\n\n\n\n")
        val z = Symbol.fresh("z") // z
        val v = Symbol.fresh("v") // z
        C.LetL(z, IntLit(0),
          C.LetP(v, MiniScalaBlockGet, Seq(name, z), ctx(v)))
      
      // case class VarDec(x: Name, xtp: Type, rhs: Tree, body: Tree) extends Tree
      case S.VarDec(name, typ, value, body) =>
        val s1 = Symbol.fresh("s") // s
        val s2 = Symbol.fresh("z") // z
        val s3 = Symbol.fresh("d") // d

        C.LetL(s1, IntLit(1),
        // object BlockTag extends Enumeration {
        //   val Pair, Array, EmptyList, List = Value
        //   val String = Value(200)
        //   val RegisterFrame = Value(201)
        //   val Function = Value(202)
        //   val Variable = Value(242)
        // }
          // case class LetP(name: Name, prim: ValuePrimitive, args: Seq[Name], body:Tree) extends Tree
          C.LetP(name, MiniScalaBlockAlloc(BlockTag.Variable.id), Seq(s1),
            // case class LetL(name: Name, value: Literal, body: Tree) extends Tree
            C.LetL(s2, IntLit(0), nonTail(value)(v => C.LetP(s3, MiniScalaBlockSet, Seq(name, s2, v), nonTail(body)(ctx)(mut + name))))))
       
      // case class VarAssign(x: Name, rhs: Tree) extends Tree
      case S.VarAssign(name, rhs) =>
        val z = Symbol.fresh("z")
        val d = Symbol.fresh("d")
        C.LetL(z, IntLit(0),
          nonTail(rhs)(v =>
            C.LetP(d, MiniScalaBlockSet, Seq(name, z, v), ctx(v))))

      case S.App(f, _, args) =>
        val r = Symbol.fresh("r");
        nonTail(f)(p =>
          nonTail_*(args)(as =>
            // tempLetC("c", Seq(r), ctx(r))(k => App(f, k, as))
            tempLetC("c", Seq(r), ctx(r))(k => 
              C.AppF(p, k, as))))

      case S.If(con, te, ee) =>
        // val r = Symbol.fresh("r")
        // tempLetC("c", Seq(r), ctx(r))(c =>
        //   tempLetC("ct", Seq(), nonTail(te)(v2 => C.AppC(c, Seq(v2))))(ct =>
        //     tempLetC("cf", Seq(), nonTail(ee)(v3 => C.AppC(c, Seq(v3))))(cf =>
        //       cond(con,ct,cf) ) ) )

        val r = Symbol.fresh("r")
        tempLetC("c",Seq(r), ctx(r))(c =>
          tempLetC("ct", Seq(), tail(te, c))(v1 =>
            tempLetC("cf", Seq(), tail(ee, c))(v2 =>
              cond(con,v1,v2) ) ) )

      // case class While(cond: Tree, lbody: Tree, body: Tree) extends Tree
      case S.While(con, lbody, body) =>
        // val loop = Symbol.fresh("loop")
        // val tempbody = tempLetC("c", Seq(), nonTail(body)(ctx))(c =>
        //   tempLetC("ct", Seq(), nonTail(lbody)(_ => 
        //     C.AppC(loop, Seq()))) (ct =>
        //       cond(con, ct, c) ) )
        // C.LetC(Seq(C.CntDef(loop, Seq(), tempbody)),C.AppC(loop, Seq()))

        val d = Symbol.fresh("d")
        val loop = Symbol.fresh("loop")
        val tempbody = tempLetC("c", Seq(), nonTail(body)(ctx))(c =>
          tempLetC("ct", Seq(), tail(lbody, loop))(ct =>
            cond(con, ct , c)
          )
        )
        // ???
        val dd = Symbol.fresh("d");
        C.LetC(Seq(C.CntDef(loop, Seq(d),tempbody)), 
          C.LetL(dd, UnitLit, C.AppC(loop, Seq(dd)))
        )

      case S.Prim(op : MiniScalaTestPrimitive, args) =>
        // case S.If(con, te, ee) =>
        nonTail(S.If(tree, S.Lit(BooleanLit(true)), S.Lit(BooleanLit(false) ) ) )(ctx)
        

      // case class Prim(op: Primitive, args: List[Tree]) extends Tree
      case S.Prim(op, args) =>
        // case class LetP(name: Name, prim: ValuePrimitive, args: Seq[Name], body:Tree)
        // MiniScalaTestPrimitive
        val p = Symbol.fresh("p");
        nonTail_*(args)(a  =>
          C.LetP(p, op.asInstanceOf[MiniScalaValuePrimitive], a, ctx(p)));
    }
  }
  
  // nonTail_* takes a sequence of S.Tree, and a continuation that takes a
  // sequence of symbols.  The sequence of symbols in the continuation
  // represents the transformed result of `trees`.  This is particularly useful
  // for the App case in nonTail.
  private def nonTail_*(trees: Seq[S.Tree])(ctx: Seq[Symbol]=>C.Tree)(implicit mut: Set[Symbol]): C.Tree =
    trees match {
      case Seq() => 
        ctx(Seq())
      case t +: ts =>
        nonTail(t)(tSym => nonTail_*(ts)(tSyms => ctx(tSym +: tSyms)))
    }

  private def tail(tree: S.Tree, c: Symbol)(implicit mut: Set[Symbol]): C.Tree = {
    // @unchecked to avoid bogus compiler warnings
    (tree: @unchecked) match {
      case S.Let(name, _, value, body) =>
        nonTail(value)(v =>
          C.LetP(name, MiniScalaId, Seq(v), tail(body, c)))

      case S.If(condE, te, ee) =>
        tempLetC("ct", Seq(), tail(te, c))(ct =>
          tempLetC("cf", Seq(), tail(ee, c))(cf =>
            cond(condE, ct, cf)))
      
      case S.LetRec(functions, body) =>
        /* TAIL */
        val funcDefs = functions map { f => 
            var fresh = Symbol.fresh("c");
            C.FunDef(f.name, fresh, f.args map { arg => arg.name}, tail(f.body, fresh))
        }
        C.LetF(funcDefs, tail(body, c))

      case S.Lit(value) =>
        val i = Symbol.fresh("i")
        C.LetL(i, value, C.AppC(c, Seq(i)))

      case S.Ref(n) if !mut(n) =>
        C.AppC(c, Seq(n))

      case S.Ref(n) if mut(n) =>
        // val z = Symbol.fresh("z") // z
        // val v = Symbol.fresh("v") // z
        // C.LetL(z, IntLit(0),
        //   C.LetP(v, MiniScalaBlockGet, Seq(name, z), ctx(v)))
        val z = Symbol.fresh("z")
        val v = Symbol.fresh("v")
        C.LetL(z, IntLit(0), C.LetP(v, MiniScalaBlockGet, Seq(n, z), C.AppC(c, Seq(v))))

      case S.VarDec(name, typ, value, body) =>
        val s1 = Symbol.fresh("s") // s
        val s2 = Symbol.fresh("z") // z
        val s3 = Symbol.fresh("d") // d

        C.LetL(s1, IntLit(1),
          C.LetP(name, MiniScalaBlockAlloc(BlockTag.Variable.id), Seq(s1),
            C.LetL(s2, IntLit(0), nonTail(value)(v => 
              C.LetP(s3, MiniScalaBlockSet, Seq(name, s2, v), tail(body, c)(mut + name))))))

      case S.VarAssign(name, rhs) =>
        val z = Symbol.fresh("z")
        val d = Symbol.fresh("d")
        C.LetL(z, IntLit(0),
          nonTail(rhs)(v =>
            C.LetP(d, MiniScalaBlockSet, Seq(name, z, v), C.AppC(c, Seq(v)))))

      case S.App(f, _, args) =>
        val r = Symbol.fresh("r");
        nonTail(f)(p =>
          nonTail_*(args)(as =>
            // tempLetC("c", Seq(r), ctx(r))(k => App(f, k, as))
            C.AppF(p, c, as)))

      case S.While(con, lbody, body) =>
        val d = Symbol.fresh("d")
        val loop = Symbol.fresh("loop")
        val tempbody = tempLetC("c", Seq(), tail(body, c))(c =>
          tempLetC("ct", Seq(), tail(lbody, loop))(ct =>
            cond(con, ct , c)
          )
        )
        val dd = Symbol.fresh("d");
        C.LetC(Seq(C.CntDef(loop, Seq(d),tempbody)), 
          C.LetL(dd, UnitLit, C.AppC(loop, Seq(dd)))
        )

      case S.Prim(op : MiniScalaTestPrimitive, args) =>
        tail(S.If(tree, S.Lit(BooleanLit(true)), S.Lit(BooleanLit(false))), c)
        

      // case class Prim(op: Primitive, args: List[Tree]) extends Tree
      case S.Prim(op, args) =>
        // case class LetP(name: Name, prim: ValuePrimitive, args: Seq[Name], body:Tree)
        // MiniScalaTestPrimitive
        val p = Symbol.fresh("p");
        nonTail_*(args)(a  =>
          C.LetP(p, op.asInstanceOf[MiniScalaValuePrimitive], a, C.AppC(c, Seq(p) ) ) );
  
    }
  }

  private def cond(tree: S.Tree, trueC: Symbol, falseC: Symbol)(implicit mut: Set[Symbol]): C.Tree = {
    def litToCont(l: CMScalaLiteral): Symbol =
      if (l != BooleanLit(false)) trueC else falseC

    tree match {
      case S.If(condE, S.Lit(tl), S.Lit(fl)) =>
        cond(condE, litToCont(tl), litToCont(fl))

      case S.If(condE, tBranch, S.Lit(fl)) =>
        // println("trueC: " + trueC);
        // println("falseC: " + falseC);
        // println("condE: " + condE);
        // println("tBranch: " + tBranch);
        // println("S.Lit(fl): " + S.Lit(fl));

        if(litToCont(fl) == falseC){
          tempLetC("ac", Seq(), cond(tBranch, trueC, falseC ))(f =>
            cond(condE, f, falseC ) )
        } else {
          tempLetC("ac", Seq(), cond(tBranch, trueC, falseC ))(f =>
            cond(condE, f, trueC ) )
        }


      case S.If(condE, S.Lit(tl), eBranch) =>
        tempLetC("ac", Seq(), cond(eBranch, trueC, falseC))(f =>
          cond(condE, litToCont(tl), f ) )

      // case S.If(condE, tBranch, eBranch) =>
      //   ???

      case S.Prim(p: MiniScalaTestPrimitive, args) =>
        nonTail_*(args)(as => C.If(p, as, trueC, falseC))

      case other =>
        nonTail(other)(o =>
          nonTail(S.Lit(BooleanLit(false)))(n =>
            C.If(MiniScalaNe, Seq(o, n), trueC, falseC)))
    }
  }

  // Helper function for defining a continuation.
  // Example:
  // tempLetC("c", Seq(r), ctx(r))(k => App(f, k, as))
  private def tempLetC(cName: String, args: Seq[C.Name], cBody: C.Tree)
                      (body: C.Name=>C.Tree): C.Tree = {
    val cSym = Symbol.fresh(cName)
    C.LetC(Seq(C.CntDef(cSym, args, cBody)), body(cSym))
  }
}
