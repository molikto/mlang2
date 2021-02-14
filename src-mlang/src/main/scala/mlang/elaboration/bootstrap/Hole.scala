package mlang.elaboration.bootstrap

import mlang.elaboration._
import dench.bootstrap._
import mlang.core.{ Set => S, _ }


given elabHole: CheckOnlyElaborator[Hole.type] with
  extension (_h: Hole.type) def check(typ: value.Term)(using ctx: Context) =
    ctx.meta(typ)

  // MAYBE without level variable we cannot do this https://github.com/AndrasKovacs/elaboration-zoo/blob/master/04-implicit-args/Elaboration.hs#L166
  // def infer(using ctx: Context) =
  //   val ss = S.Zero
  //   val typ = ctx.meta(ss).eval
  //   val m = ctx.meta(typ)
  //   (m, typ)