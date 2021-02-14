package mlang.elaboration.bootstrap

import utils._
import mlang.elaboration._
import mlang.core._
import mlang.core.translate.eval
import mlang.core.syntax.dbi.shift
import dench.bootstrap._

given elabMatch: CheckOnlyElaborator[Match] with
  extension (thiz: Match) def check(typ: value.Term)(using Context): syntax.Term =
    val Match(args, lam) = thiz
    if args.size != 1 then notHandled()
    val arg: Term = args(0)
    val (s, t2) = arg.infer
    val ss = lam.check(value.Pi(t2, _ => typ, etyp.Pi("")))
    syntax.App(ss, s)