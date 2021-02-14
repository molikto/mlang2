package mlang.elaboration

import utils._
import mlang.infra._
import mlang.core._
import mlang.core.translate._
import mlang.core.value.{ Def, Meta, Inversable, ConversionChecker, MetaGenericsApps, Generic, SkelGlue, Axiom }
import mlang.core.syntax.dbi.{ shift, lift, InsertInfo }
import scala.collection.immutable
import scala.util.Random
import mlang.core.syntax.skel
import dench.bootstrap.BoxType

case class NoExtensionException(val ref: Ref) extends Exception(ref.toString)
case class ExtensionAmbiguousException(val ref: Ref) extends Exception
case class NotResolvedException(val ref: Ref) extends Exception(ref.toString)
class MetaNotSolvedException() extends Exception
class LiftingNotAllowedException() extends Exception
class DuplicatedGlobalDefinitionException(val ref: Ref) extends Exception(ref.toString)

class MetaSolveNonLinearException() extends Exception

type ExtendHandler[T] = (value.Term) => Context ?=> T

type Snapshot = Seq[Seq[value.Term | Null]] // FIXME very hacky way

enum SpecialRef:
  case Declared
  case Axiom
  case Make
  case Error

case class Context private[elaboration] (
  val namespace: Ref = Seq.empty,
  val defs: immutable.Map[Ref, GlobalLayer] = immutable.Map.empty,
  val extensions: immutable.MultiDict[Name, GlobalLayer] = immutable.MultiDict.empty,
  val layers: Seq[LocalLayer] = Seq.empty
) extends ConversionChecker with checker.Context with ReifyContext:
  def withNamespace(ns: Ref): Context =
    copy(namespace = ns)
  def pushNamespace(ns: Ref): Context =
    copy(namespace = namespace + ns)
  def popNamespace(ns: Ref): Context =
    copy(namespace = namespace.pop(ns))

  //
  // for checker.Context
  //
  override def apply(r: syntax.Ref): value.Inversable = 
    val i = layers.size - r.get
    if i >= 0 then
      val t = layers(i).term
      if t != null then t else logicError()
    else logicError()

  def apply(r: syntax.TempRef): value.Meta =
    val l = layers(r.i1)
    l.metas(r.i2)

  def apply(r: syntax.GlobalRef): Inversable = 
    val ret = getDef(r.name, r.l)
    if ret != null then ret
    else throw NotResolvedException(r.name)

  def size = layers.size

  //
  // naming
  //
  private def specialRef(t: Ref): SpecialRef | Null = 
    t match
    case r: Name =>
      if r == "declared" then SpecialRef.Declared
      else if r == "axiom" then SpecialRef.Axiom
      else if r == "make" then SpecialRef.Make
      else if r == "error" then SpecialRef.Error
      else null
    case _ => null

  private def getDef(ref: Ref, l: Int): value.Inversable | Null =
    defs.get(ref) match
    case Some(r) =>
      r match
      case layer: DeclLayer => if l == 0 then layer.term else throw LiftingNotAllowedException()
      case d: DefLayer => d(l)
    case _ => null


  def apply(ref0: Ref, l: Int): (syntax.Term, value.Term) | SpecialRef | Null =
    def fallback(): (syntax.Term, value.Term) | SpecialRef | Null = 
      var ns: Ref | Null = namespace
      var ref = ref0
      var ret: value.Inversable | Null = null
      while (ret == null && ns != null) {
        ref = ns + ref0
        ret = getDef(ref, l)
        if ns.nonEmpty then ns = ns.pop1
        else ns = null
      }
      if ret != null then
        (syntax.GlobalRef(ref, l), ret)
      else if ref0 == "set" then (Set(l), Set(l))
      else if ref0 == "prop" then (Prop(l), Prop(l))
      else 
        val sp = specialRef(ref0)
        if l == 0 && sp != null then sp
        else null
    ref0 match
    case ref: Name => 
      val index = layers.lastIndexWhere(l => l.name == ref)
      if index >= 0 then
        if l == 0 then (syntax.Ref(layers.size - index), layers(index).term.nn)
        else throw LiftingNotAllowedException()
      else fallback()
    case _ => fallback()

  def applyExtension[T](ref0: Ref, l: Int, pred: (syntax.Term, value.Term) => T | Null): T = 
    val ref = if ref0.isInstanceOf[Name] then ref0 else notHandled() // TODO scoped extension applying
    val t = extensions.get(ref.last)
    if t.isEmpty then throw NoExtensionException(ref)
    else
      var res: T | Null = null
      t.foreach(a => {
        val aaa = apply(a.ref, l) match
        case (aa, bb) =>
          val neu = pred(aa, bb)
          if neu != null && res != null then throw ExtensionAmbiguousException(ref)
          else if neu != null then res = neu
        case _ =>
      })
      if res == null then throw NoExtensionException(ref)
      res.asInstanceOf[T]

  def snapshot(): Snapshot = layers.map(_.metas.map(_.term).toSeq)

  def rollback(s: Snapshot) =
    contract { s.size == layers.size }
    s.zip(layers).foreach(pair => {
      pair._2.metas.remove(pair._1.size, pair._2.metas.size - pair._1.size)
      pair._1.zip(pair._2.metas).foreach(pair => {
        pair._2.term = pair._1
      })
    })



  //
  // for ReifyContext
  //
  def inverse(v: value.Inversable): syntax.RefLike | Null =
      var ret: syntax.RefLike | Null = null
      var i = layers.size
      while ret == null && i > 0 do
        i -= 1
        val l = layers(i)
        val res = l.inverse(v)
        if res == -1 then
          ret = syntax.Ref(layers.size - i)
        else if res >= 0 then
          ret = syntax.TempRef(i, res)
      if ret == null then
        defs.foreach(p => {
          if ret == null then
            val l = p._2.inverse(v)
            if l != -1 then ret = syntax.GlobalRef(p._1, l)
        })
      ret
  
  //
  // for conversion checker
  //
  override protected def equalsStuckWhnf(w1: value.Term, w2: value.Term): value.Term | Null = 
    val su = super.equalsStuckWhnf(w1, w2)
    if su != null then su else
      (w1, w2) match
      case (MetaGenericsApps(m, as), b) => solve(m, as, b)
      case (b, MetaGenericsApps(m, as)) => solve(m, as, b)
      case _ =>
        null
  
  def trySolve(m: Meta, as: Seq[Generic], b: value.Term): value.Term | Null = 
    try
      solve(m, as, b)
    catch
    case _: MetaSolveNonLinearException => null
    case _: NotResolvedException => null
    
  def solve(m: Meta, as: Seq[Generic], b: value.Term): value.Term =
    trace.log(s"solve $m $as $b")
    inverse(m) match
    case syntax.TempRef(i1, i2) =>
      var ctx: Context = copy(layers = layers.take(i1 + 1))
      val ctx0 = ctx
      val linear1 = as.forall(a => ctx.inverse(a) == null)
      val linear2 = as.distinct.size == as.size
      if linear1 && linear2 then
        ctx = as.foldLeft(ctx) { (c, a) => c.newLayer("", a) }
        var typ = m.typ
        var syn = b.reify(using ctx)
        import mlang.core.value.whnf
        import checker.check
        for (a <- as) {
          typ = typ.whnf match
          case value.Unwrap(value.Pi(_, c, _)) => c(a)
          case _ => logicError()
        }
        syn.check(typ)(using ctx) // must modify after check... bad
        for (a <- as) {
          syn = syntax.Lambda(syntax.Closure(syn))
        }
        m.term = syn.eval(using ctx0)
        typ
      else throw MetaSolveNonLinearException()
    case _ => logicError()
  
  // construction

  def finish(): InsertInfo | Null =
    layers.lastOption match
    case Some(layer) =>
      val ms = layer.finish()
      if ms.exists(_.term == null) then throw MetaNotSolvedException()
      if ms.size == 0 then null else
        value.Make(ms.toSeq).reify(using this) match
          case syntax.Let(ss, syntax.Make(vs)) =>
            InsertInfo(size - 1, vs.map(vs.size - _.asInstanceOf[syntax.Ref].get), ss)
          case _ => logicError()
    case _ => logicError() // remove this
  
  def newDummyLayer(): Context =
    copy(layers = layers :+ DummyLayer())

  def newLayer(name: Name, term: Inversable): Context =
    copy(layers = layers :+ GenericLayer(name, term))

  def newExtensionDecl(name: Name, typ: value.Term): (Ref, Context) =
    val gen = value.Def(typ, Generic(typ))
    val key = this.namespace + Seq(Random.nextLong().toString: Name, name)
    val layer = DeclLayer(key, gen)
    val gs = extensions + (name -> layer)
    (key, copy(extensions = gs, defs = defs + (key -> layer)))

  def newExtension(name: Name, key: Ref, typ: syntax.Term, body: syntax.Term | Null): Context =
    contract { layers.isEmpty }
    val layer = DefLayer(this, key, typ, body)
    val gs = extensions + (name -> layer)
    val ctx = copy(extensions = gs, defs = defs + (key -> layer))
    layer.ctx = ctx
    ctx

  def newGlobalDecl(name: Name, typ: value.Term): Context =
    contract { layers.isEmpty }
    val full = namespace + name
    defs.get(full) match
    case Some(_) =>
      throw DuplicatedGlobalDefinitionException(full)
    case _ => 
      val gen = value.Def(typ, Generic(typ))
      val layer = DeclLayer(full, gen)
      val gs = defs + (full -> layer)
      copy(defs = gs)

  def newGlobalDef(keyword: BoxType, name: Name, typ: syntax.Term, body: syntax.Term | Null): Context =
    contract { layers.isEmpty }
    val full = namespace + name
    defs.get(full) match
    case Some(d: DefLayer) =>
      throw DuplicatedGlobalDefinitionException(full)
    case _ => 
      val layer = DefLayer(this, full, typ, body)
      val gs = defs + (full -> layer)
      val ctx = copy(defs = gs)
      layer.ctx = ctx
      ctx

  def meta(typ: value.Term): syntax.TempRef =
    val md = new value.Meta(typ, null)
    trace.log(s"meta $md ${layers.size}")
    layers.last.metas += md
    syntax.TempRef(layers.size - 1, layers.last.metas.size - 1)

object Context:
  def apply(): Context = 
    val ctx = bootstrap.check(Seq.empty, dench.bootstrap.parse(builtIns))(using new Context())
    def intro(name: String): value.BuiltIn = (i: Int) => ctx(name, i).asInstanceOf[(syntax.Term, value.Term)]._2
    value.top = intro("⊤")
    value.bottom = intro("⊥")
    value.refl = intro("refl")
    value.sym = intro("sym")
    value.trans = intro("trans")
    value.ap = intro("ap")
    ctx


//
// helpers
//
def dummyGeneric[T]()(handler: ExtendHandler[T])(using ctx: Context): (InsertInfo | Null, T) =
  val i1 = ctx.layers.size
  val term = Generic(Prop.Zero)
  val ctx1 = ctx.newDummyLayer()
  val t = handler(term)(using ctx1)
  val ms = ctx1.finish()
  (ms, t)

def generic[T](name: Name, typ: value.Term)(handler: ExtendHandler[T])(using ctx: Context): (InsertInfo | Null, T) =
  val i1 = ctx.layers.size
  val term = Generic(typ)
  val ctx1 = ctx.newLayer(name, term)
  val t = handler(term)(using ctx1)
  val ms = ctx1.finish()
  (ms, t)

def local[T](name: Name, typ: value.Term, term: value.Term)(using ctx: Context): Context =
  // TODO warn same name?
  val gen = value.Def(typ, term)
  ctx.newLayer(name, gen)
