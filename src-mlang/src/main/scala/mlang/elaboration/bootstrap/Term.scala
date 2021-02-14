package mlang.elaboration.bootstrap

import utils._
import mlang.infra._
import mlang.core.{ syntax, value, etyp }
import mlang.core.translate.eval
import mlang.core.syntax.dbi.shift
import mlang.core.value.whnf
import mlang.core.translate.reify
import mlang.elaboration._
import dench.bootstrap._

def insertImplicitApps(res: InferResult, until: Plicity)(using ctx: Context): InferResult =
  trace.enter(s"insert implicits $until")
  val ret = res._2.whnf match
  case value.Unwrap(value.Pi(a, b, etyp.Pi(i))) if i.plicity != until => 
    i.plicity match
    case Plicity.Im => 
      trace.log(s"instinated meta with $i and $a")
      val s = ctx.meta(a)
      val res1 = (syntax.App(res._1, s), b(s.eval))
      insertImplicitApps(res1, until)
    case Plicity.Instance =>
      ???
    case _ => res
  case _ => res
  trace.exit()
  ret

trait ImplicitAwareInferableElaborator[T <: Term] extends InferableElaborator[T]:
  extension (thiz: T) override def check(typ: value.Term)(using ctx: Context): syntax.Term = 
    val (s, t) = insertImplicitApps(thiz.infer, Plicity.Ex)
    trace.enter(s"fallback checker $s $typ")
    val res = if ctx.subtyp(t, typ) then s else
      // println(thiz)
      // println(s)
      // println(ctx)
      // println(t.whnf)
      // println(typ.whnf)
      // println(t.reify)
      // println(typ.reify)
      // println(t.whnf.reify)
      // println(typ.whnf.reify)
      throw ConversionFailedException("")
    trace.exit()
    res

given elabTerm: Elaborator[Term] with
  extension (thiz: Term) def infer(using Context) =
    trace.enter(s"infer $thiz")
    val res = try {
      thiz match
      case t: Ref => elabRef.infer(t)
      case t: LiftRef => elabRef.infer(t)
      case t: Proj => elabProj.infer(t)
      case t: Lambda => elabLambda.infer(t)
      case t: Pi => elabPi.infer(t)
      case t: Match => elabMatch.infer(t)
      case t: Eq => elabEq.infer(t)
      case t: Record => elabRecord.infer(t)
      case t: Enum => elabEnum.infer(t)
      case t: Let => elabLet.infer(t)
      case t: App => elabApp.infer(t)
      case Hole => elabHole.infer(Hole)
      case t: At =>
        val (s, _) = t.right.inferAsTyp
        val _s = s.eval
        (t.left.check(_s), _s)
      case t: Int => elabNat.infer(t)
    } catch {
      case e: Exception => 
        println("failed to infer " + thiz)
        throw e
    }
    trace.exit(s"infered $res")
    res


  extension (thiz: Term) def check(typ: value.Term)(using ctx: Context): syntax.Term =
    trace.enter(s"check ${ctx.layers.size} $thiz " + typ.reify)
    val res = try {
    thiz match
    case t: Lambda => elabLambda.check(t)(typ)
    case t: Match => elabMatch.check(t)(typ) // TODO mmm... ok not sure if we should use this branch
    case _ => 
      typ.whnf match
      case value.Unwrap(value.Pi(a, b, etyp.Pi(i))) if i.plicity.impli => 
        val (ms, s) = generic("", a) { gen => thiz.check(b(gen)) }
        syntax.Lambda(syntax.Closure(s.shift(0, ms)))
      case _ => 
        thiz match
        case t: Ref => elabRef.check(t)(typ)
        case t: LiftRef => elabRef.check(t)(typ)
        case t: Proj => elabProj.check(t)(typ)
        case t: Pi => elabPi.check(t)(typ)
        case t: Eq => elabEq.check(t)(typ)
        case t: Record => elabRecord.check(t)(typ)
        case t: Enum => elabEnum.check(t)(typ)
        case t: Let => elabLet.check(t)(typ)
        case t: App => elabApp.check(t)(typ)
        case Hole => elabHole.check(Hole)(typ)
        case t: Int => elabNat.check(t)(typ)
        case t: At =>
          val (s, _) = t.right.inferAsTyp
          val _s = s.eval
          val syn = t.left.check(_s)
          if ctx.subtyp(_s, typ) then syn else throw ConversionFailedException("")
        case t: Match => logicError()
        case t: Lambda => logicError()
    } catch {
      case e: Exception => 
        trace.exit("check failed")
        throw e
    }
    trace.exit()
    res