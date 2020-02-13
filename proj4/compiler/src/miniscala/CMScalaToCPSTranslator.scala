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
      case S.LetRec(functions, body) =>
        // println("AEWFEAFWFFFFW\n\n\n\n\n\n");
        ???

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


      case S.LetRec(functions, body) =>
        println("EWAFAWEFAWEGFEWAGF\n\n\n\n\n\n\n\n\n\n");
        ???
      // TODO: add the missing cases.
    }
  }

  private def cond(tree: S.Tree, trueC: Symbol, falseC: Symbol)(implicit mut: Set[Symbol]): C.Tree = {
    def litToCont(l: CMScalaLiteral): Symbol =
      if (l != BooleanLit(false)) trueC else falseC

    tree match {
      case S.If(condE, S.Lit(tl), S.Lit(fl)) =>
        cond(condE, litToCont(tl), litToCont(fl))

      // TODO add missing cases

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
