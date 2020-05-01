1.1
def eval(e: Exp)(env: ValueEnv): Val = e match { 
        case Throw(n) => Fail(n, List()) 
        case TryCatch(e, h) => 
            val ret = eval(e)(env);
            ret match {
                case Fail(n, l) => eval(h[n])(env)
                case f@_ => f
            }
        case Finally(ebody, efin) =>
            var ret = eval(ebody)(env) 
            eval(efin)(env) match {
                case Cst(u: Unit) => ret
                case f@Fail(n, l) => f
                case _ => throw SemanticError("unit expected")
            }
        case App(efun, earg) => 
            eval(efun)(env) match {
                case f@Fail(n, l) => f
                case Func(arg, fbody, envf) => 
                    val ret = eval(earg)(env)
                    ret match {
                        case f@Fail(n, l) => f
                        case _ =>
                            val ret2 = eval(fbody)(fenv.withVal(arg))
                            val ret3 = ret2 match {
                                case Fail(n, l) => Fail(n, List(l += fbody.pos))
                                case f@_ => f
                            }
                            ret3
                    }
                case _ => throw SemanticError("expected function value") 
            } 
}

1.2 
def evalk(e: Exp, env: ValueEnv)(ks: Val => Unit)(kf : (Exc, STrace) => Unit): Unit = e match {
    case Throw(n) => kf(n, List())
    case Trycatch(e1, h) =>
        evalk(e1, env)(ks){(excItem, _) => evalk(h(excItem), env)(ks)(kf)}
    case Finally(ebody, efin) => 
        evalk(ebody, env)
            { (valL) => evalk(efin, env)(ks(valL))(kf) }
            { (extT, sT) => evalk(efin, env){ (v2) => kf(extT, sT) } { (extT2, sT2) => kf(extT2, sT2) } }
    case App(efun, earg) =>
        evalk(efun, env) {(funF) => { evalk(earg, env) {v1 => evalk(funF.fbody, funF.envf.withVal(funF.arg, v1))(ks)((nn2, ss2)=> kf(nn2, List(funF.fbody.pos += ss2)) } (kf) }} (kf)
}


1.3 
Throw:
    Γ ⊢ n: Int
-------------------
Γ ⊢ Throw(n) : Fail

TryCatch:
    Γ ⊢ e0 : T0  Γ ⊢ n1 : Int · · · Γ ⊢ nk : Int  Γ ⊢ e1 : T1 · · · Γ ⊢ ek : Tk 
---------------------------------------------------------------------------
            Γ ⊢ TryCatch(e0, n1 → e1 · · · nk → ek): (T0 | · · · | TK)

(T0 | · · · | TK) = Any of the T's can be the return type of TryCatch.

Finally: 
  Γ ⊢ e1 : T0   Γ ⊢ e2: T1
-----------------------------
  Γ ⊢ Finally(e1, e2): T0

2. 
    ready           active
-----------------------------
0  [a,e,g]      [a]
1  [e,g]        [a,e]
2  [g]          [a,e,g]
3  [b,c]        [b,e,g]
4  [c]          [c,b,g]
5  []           [c,b]
6  []           [c]
7  [d]          [d]
8  [f]          [f]
9  []           [f]
10 []           [f]
11 [h]          [h]
12 [i]          [i]
13 []           [i]
14 []           [i]
15 []           []