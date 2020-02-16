package miniscala

import miniscala.{ SymbolicCMScalaTreeModule => S }
import miniscala.{ SymbolicCPSTreeModule => C }

object CMScalaToCPSTranslator extends (S.Tree => C.Tree) {
  def apply(tree: S.Tree): C.Tree = {
    nonTail(tree){ ret =>
      val z = Symbol.fresh("c0")
      C.LetL(z, IntLit(0), C.Halt(ret)) // TODO REVERT TO C.Halt(z)
      // C.LetL(z, IntLit(0), C.Halt(z))
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
        println("AEWFEAFWFFFFW " + functions + "\n");
        val funcDefs = functions map { f => 
            var fresh = Symbol.fresh("rc");
            C.FunDef(f.name, fresh ,f.args map { arg => arg.name}, nonTail(f.body)(v => C.AppC(fresh, Seq(v))))
        }
        println("AEWFEAFWFFFFW " + funcDefs + "\n\n\n\n\n\n");
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
            tempLetC("c", Seq(r), ctx(r))(k => C.AppF(p, k, as))))

      case S.If(con, te, ee) =>
        val r = Symbol.fresh("r")
        tempLetC("c", Seq(r), ctx(r))(c =>
          tempLetC("ct", Seq(), nonTail(te)(v2 => C.AppC(c, Seq(v2))))(ct =>
            tempLetC("cf", Seq(), nonTail(ee)(v3 => C.AppC(c, Seq(v3))))(cf =>
              cond(con,ct,cf) ) ) )

    // case class FunDef(name: Name, ptps: List[String], args: List[Arg], rtp: Type, body: Tree)
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

      case S.If(condE, thenE, elseE) =>
        tempLetC("cf", Seq(), tail(thenE, c))(cf =>
          tempLetC("cf", Seq(), tail(elseE, c))(cf =>
            cond(condE, cf, cf)))
      
      case S.LetRec(functions, body) =>
        println("EWAFAWEFAWEGFEWAGF\n\n\n\n\n\n\n\n\n\n");
        ???

      case S.Lit(value) =>
        val i = Symbol.fresh("i")
        C.LetL(i, value, C.AppC(c, Seq(i)))
    }
  }

  private def cond(tree: S.Tree, trueC: Symbol, falseC: Symbol)(implicit mut: Set[Symbol]): C.Tree = {
    def litToCont(l: CMScalaLiteral): Symbol =
      if (l != BooleanLit(false)) trueC else falseC

    tree match {
      case S.If(condE, S.Lit(tl), S.Lit(fl)) =>
        cond(condE, litToCont(tl), litToCont(fl))

      // case S.If(condE, thenE, S.Lit(l)) =>
      //   tempLetC("tc", Seq(), cond(thenE, trueC, falseC))(tc =>
      //     cond(condE, tc, litToCont(l)))

      // case S.If(condE, S.Lit(l), elseE) =>
      //   tempLetC("ec", Seq(), cond(elseE, trueC, falseC))(ec =>
      //     cond(condE, litToCont(l), ec))

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
