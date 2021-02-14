package mlang.core.syntax

import mlang.core._
import utils.{ logicError, contract }

def noValidSkel(): Nothing = throw new Exception()

extension (term: Term) def skel(rec: Boolean, valid: Boolean = false, shallow: Boolean = true) = SkelChecker(valid, shallow).skel(term, rec)

/**
  * it basically checks pattern lambdas doesn't appear in the syntax except several allowed pattern
  * (in a more powerful system this check might be encoded at type level?
  * maybe Scala can already do this? but I don't want the trouble setting it up)
  *
  * @param valid ensured valid skel, no check performed
  * @param shallow defs inside are valid syntax with valid skel
  */
class SkelChecker(valid: Boolean, shallow: Boolean):
  private def checkNoSkel(tele: Telescope): Unit =
    tele match
    case Telescope.Nil =>
    case Telescope.Let(defs, in) => defs.foreach(checkNoSkel); checkNoSkel(in)
    case Telescope.Cons(head, tail) => checkNoSkel(head); checkNoSkel(tail.get)

  private def checkNoSkel(d: Box.Local): Unit =
    if !shallow then
      d match
      case Box.Def(tp, tm) =>
        checkNoSkel(tp)
        skel(tm, false)
      case Box.Meta(tp, tm) =>
        checkNoSkel(tp)
        checkNoSkel(tm)

  def skel(term: Term, rec: Boolean): Skel | Null =
    term match
    case Lambda(b) =>
      skel(b.get, rec) match
      case sk: Skel => Skel.Lambda(sk)
      case _ => null
    case PatternLambda(bs) =>
      Skel.PatternLambda(bs.map(b => skel(b.clos.get, rec)))
    case Record(fs, _) =>
      if (!valid) checkNoSkel(fs)
      if rec then
        Skel.Inductive
      else null
    case Enum(ks, _) =>
      if (!valid) ks.foreach(checkNoSkel)
      if rec then Skel.Inductive else null
    case a =>
      if (!valid) checkNoSkel(a)
      null
  
  def checkNoSkel(term: Term): Unit = 
    term match
      case Pi(d, c, _) => checkNoSkel(d); checkNoSkel(c.get)
      case Eq(a, b, c) => checkNoSkel(a); checkNoSkel(b); checkNoSkel(c)
      case Record(fs, _) => checkNoSkel(fs)
      case Enum(ks, _) => ks.foreach(checkNoSkel)
      case Lambda(b) => checkNoSkel(b.get)
      case p: PatternLambda => noValidSkel()
      case Construct(_, fs) => fs.foreach(checkNoSkel)
      case Make(fs) => fs.foreach(checkNoSkel)
      case App(l, r) => checkNoSkel(l); checkNoSkel(r)
      case Proj(l, _) => checkNoSkel(l)
      case Let(ds, in) => ds.foreach(checkNoSkel); checkNoSkel(in)
      case _: Ref =>
      case _: TempRef =>
      case Error =>
      case _: Sort =>
      case a: GlobalRef =>
