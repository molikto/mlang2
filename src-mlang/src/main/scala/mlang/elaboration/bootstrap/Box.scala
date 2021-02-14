package mlang.elaboration.bootstrap

import utils._
import mlang.elaboration._
import mlang.core.{ Set => S, _ }
import mlang.core.translate.eval
import mlang.core.syntax.dbi.shift
import mlang.core.syntax.skel
import dench.bootstrap._
import mlang.infra._
import mlang.core.translate.reify
import scala.collection.mutable

// FIXME NOT WORKING!!!
extension (d: Def) def typOrHole: Term = if d.typ == null then Hole else d.typ


def unwrapUnders(bs: Seq[Box]): Seq[Box] =
  bs.flatMap(_ match {
    case u: Under =>
      unwrapUnders(u.boxes).map(_ match {
        case b: Def => b.copy(intros = u.intros ++ b.intros)
        case _ => logicError()
      })
    case a => Seq(a)
  })
def wrapInLambda(bs: Seq[BoxOrField], term: Term): Term =
  if (bs.isEmpty) then term else Lambda(Seq(Branch(bs.flatMap(_ match {
    case NamesTyp(ns, _) => ns.asInstanceOf[Seq[PlicitPattern]]
    case _ => Seq.empty
  }), term)))

def unfoldIntros(d: Def): Def =
  Def(
    d.keyword,
    d.name,
    Seq.empty,
    if d.intros.isEmpty then d.typ else Pi(d.intros, d.typ.nn.asInstanceOf[Term]),
    wrapInLambda(d.intros, d.body)
  )

def checkLocalBox(d0: Box)(using ctx: Context): (syntax.Box.Local, Context) =
  checkLocalBoxInner(d0, ctx)

def checkLocalBoxInner(d0: Box, ctx: Context): (syntax.Box.Local, Context) =
  d0 match
  case d0: Def => 
    val d = unfoldIntros(d0)
    var ctx1 = ctx.newDummyLayer()
    val (hts0, htt) = d.typOrHole.inferAsTyp(using ctx1)
    val ms0 = ctx1.finish()
    val hts = hts0.shift(-1, ms0)
    val hts_ = hts0.eval(using ctx1) // TODO somehow shifting then eval has error????
    ctx1 = ctx.newDummyLayer()
    val tm0 = d.body.check(hts_)(using ctx1)
    val ms1 = ctx1.finish()
    val tm = tm0.shift(-1, ms1)
    val ctxr = local(d.name, hts_, tm0.eval(using ctx1))(using ctx)
    (syntax.Box.Def(hts, tm), ctxr)
  case _ => logicError()

private def checkLocalBoxes(defs: Seq[Box])(using ctx: Context): (Seq[syntax.Box.Local], Context) = 
  var ctx1 = ctx
  var ms = Seq.empty[syntax.Box.Local]
  unwrapUnders(defs).foreach(d => {
    trace.enter(s"local box $d")
    val (msn, ctxn) = checkLocalBoxInner(d, ctx1)
    trace.exit()
    ms = ms :+ msn
    ctx1 = ctxn
  })
  (ms, ctx1)

type DeclsCache = mutable.Map[Name, (syntax.Term, value.Term)]

private def checkDef(ctx: Context, d: Def, declsCache: DeclsCache): Context =
  if d.typ == null then
    declsCache.get(d.name) match
    case Some(pair) =>
      val ctx2 = ctx.newDummyLayer()
      val tm0 = d.body.check(pair._2)(using ctx2)
      val ms = ctx2.finish()
      val rs = tm0.shift(0, ms)
      ctx.newGlobalDef(d.keyword, d.name, pair._1, rs)
    case None =>
      val ctx1 = ctx.newDummyLayer()
      val (s, t) = d.body.infer(using ctx1)
      val ms = ctx1.finish()
      val ref = d.name
      if ref != "" then ctx.newGlobalDef(d.keyword, ref, t.reify(using ctx), s.shift(0, ms))
      else ctx
  else checkDefInner(ctx, d, declsCache)

private def checkDefInner(ctx: Context, d0: Def, declsCache: DeclsCache): Context = 
  trace.enter("checking def " + d0.name)
  trace.enter("typ")
  val d = unfoldIntros(d0)
  val ctx1 = ctx.newDummyLayer()
  val (hts0, htt) = d.typOrHole.inferAsTyp(using ctx1)
  val ms0 = ctx1.finish()
  val hts = hts0.shift(0, ms0)
  trace.exit(hts.toString)

  trace.enter("body")
  val name = d.name
  def normalCase() = 
    val hts_ = hts.eval(using ctx)
    val ctx2 =
      if name != "" then ctx.newGlobalDecl(name, hts_).newDummyLayer()
      else ctx.newDummyLayer()
    val tm0 = d.body.check(hts_)(using ctx2)
    val ms = ctx2.finish()
    val rs = tm0.shift(0, ms)
    trace.log(rs.toString)
    if name != "" then ctx.newGlobalDef(d.keyword, name, hts, rs)
    else ctx
  val res = d.body match
  case r: Ref =>
    ctx(r, 0) match
    case SpecialRef.Axiom => ctx.newGlobalDef(d.keyword, name, hts, null)
    case SpecialRef.Declared =>
      if name == "" then notHandled()
      val ttt = hts.eval(using ctx)
      declsCache += name -> (hts, ttt)
      ctx.newGlobalDecl(name, ttt)
    case _=> normalCase()
  case _ => normalCase()
  trace.exit()
  trace.exit()
  res

private def checkExtension(ctx: Context, ext: Def): Context =
  val ref = ext.name
  if ref == "" then notHandled()
  trace.enter(s"checking extension $ext")
  trace.enter(s"type")
  val pi = Pi(ext.intros, ext.typ.asInstanceOf[Term])
  val ctx1 = ctx.newDummyLayer()
  val (pis, ptv) = pi.infer(using ctx1)
  val ms0 = ctx1.finish()
  val hts = pis.shift(0, ms0)
  trace.exit()
  trace.enter(s"checking extension body")
  val hts_ = hts.eval(using ctx)
  val (key, ctx2p) = ctx.newExtensionDecl(ref, hts_)
  val ctx2 = ctx2p.newDummyLayer()
  val lam: Term = wrapInLambda(ext.intros, ext.body)
  val tm0 = lam.check(hts_)(using ctx2)
  val ms = ctx2.finish()
  val rs = tm0.shift(0, ms)
  val res = ctx.newExtension(ref, key, hts, rs)
  trace.exit()
  trace.exit()
  res


def checkNamespace(n: Namespace, ctx: Context): Context =
  checkGlobals(n.boxes)(using ctx.pushNamespace(n.ref)).popNamespace(n.ref)

def checkGlobalBoxes(defs: Seq[Box], ctx: Context, declsCache: DeclsCache): Context =
  var ctx1 = ctx
  defs.foreach(d => {
    d match
    case d: Def if d.keyword == BoxTypeExtension =>
      ctx1 = checkExtension(ctx1, d)
    case d: Def =>
      ctx1 = checkDef(ctx1, d, declsCache)
    case _: Namespace => notHandled()
    case _: Under => logicError()
  })
  ctx1

def checkGlobals(defs: Seq[Box])(using ctx: Context): Context = 
  var ctx1 = ctx
  val declsCache: DeclsCache = mutable.Map.empty
  defs.foreach(_ match {
    case n: Namespace =>
      ctx1 = checkNamespace(n, ctx1)
    case b: Box =>
      ctx1 = checkGlobalBoxes(unwrapUnders(Seq(b)), ctx1, declsCache)
  })
  ctx1

