package mlang.core.syntax.dbi

import mlang.infra._
import mlang.core.syntax._
/*
/////////////// 3 .      2.       1.
def if(#T: set, b: bool, left: T, right: T): T :=
  match b
  { true -> left
  | false -> right
  Lambda(App(Pt, 1))
// L(L(L(A(P(B(p0,2),B(p1,1))),3)))
// P(B(p0,L(L(2-p0.len))),B(p1,L(L(1-p1.len))))
*/
private[syntax] def wrapInLambda(b: Term, n: Int): Term = if n == 0 then b else wrapInLambda(Lambda(Closure(b)), n - 1)
private[syntax] def liftImmAppUp(ori: Term, term: Term, canFoldCount: Int): Term =
  term match
  case App(l, r: Ref) if r.get == canFoldCount => 
    l match
    case PatternLambda(bs) => 
      val ret = PatternLambda(bs.map(b => Branch(b.pattern, Closure(wrapInLambda(b.clos.get.shift(-1, null, Reorder(b.pattern.size, b.pattern.size + canFoldCount - 1)), canFoldCount - 1)))))
      trace.log(s"reorder lambda $ori $ret")
      ret
    case Lambda(b) =>
      val ret = Lambda(Closure(wrapInLambda(b.get.shift(-1, null, Reorder(1, canFoldCount)), canFoldCount - 1)))
      trace.log(s"reorder lambda $ori $ret")
      ret
    case _ => ori
  case Lambda(term) => liftImmAppUp(ori, term.get, canFoldCount + 1)
  case _ => ori
