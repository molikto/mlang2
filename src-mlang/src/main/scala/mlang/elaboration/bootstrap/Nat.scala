package mlang.elaboration.bootstrap

import utils._
import mlang.core.{ Set => S, Prop => P, _ }
import mlang.elaboration._
import mlang.core.translate.eval
import mlang.core.value.whnf
import dench.bootstrap._

given elabNat: ImplicitAwareInferableElaborator[Int] with
  extension (nat: Int) def infer(using ctx: Context) =
    if nat < 0 then notHandled()
    val (s, _) = ctx("nat", 0) // TODO builtin
    (natLit(nat), s.eval)

def natLit(a: Int): syntax.Term =
  var i = 0
  var n = syntax.Construct(0, Seq.empty)
  while i < a do
    n = syntax.Construct(1, n)
    i += 1
  n
