package mlang.elaboration.bootstrap

import utils._
import mlang.elaboration._
import mlang.core.syntax.dbi.shift
import mlang.core._
import mlang.core.translate.eval
import dench.bootstrap._


given elabPi: ImplicitAwareInferableElaborator[Pi] with
  extension (pi: Pi) def infer(using Context) =
    val ts = pi.dom.nameTyps :+ NameTyp("", pi.cod)
    inferPi(ts.head._1, ts.head._2, ts.tail)

private def inferPi(name: PlicitName, dom: Term, cod: Seq[NameTyp])(using Context): InferAsTypResult =
  val res = dom.inferAsTyp
  cod match
  case h +: tail => 
    val (as, at) = dom.inferAsTyp
    val (ms, (bs, bt): InferAsTypResult) = generic(name.unwrap, as.eval) { gen => inferPi(h.name, h.typ, tail) }
    (syntax.Pi(as, syntax.Closure(bs.shift(0, ms)), etyp.Pi(name)), Sort.pi(at, bt))
  case _ => res