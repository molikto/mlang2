package mlang.elaboration.bootstrap

import mlang.elaboration._
import mlang.core._
import dench.bootstrap._
import mlang.core.value.conversion
import mlang.core.translate.reify

given elabEq: ImplicitAwareInferableElaborator[Eq] with
  extension (eq: Eq) def infer(using ctx: Context) =
    val (s1, t1) = eq.left.infer
    val (s2, t2) = eq.right.infer
    if ctx.subtyp(t1, t2) then (syntax.Eq(t2.reify, s1, s2), Prop(t2.sort.l)) // FEATURE two dir subtyping?
    else if ctx.subtyp(t2, t1) then (syntax.Eq(t1.reify, s1, s2), Prop(t1.sort.l))
    else notHandled()
      