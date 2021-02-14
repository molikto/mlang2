package mlang.core

import utils._

trait HasTyp:
  def typ: value.Term  = logicError()
  def sort: Sort = 
    typ match
    case s: Sort => s
    case _ => logicError()

sealed trait Sort extends HasTyp derives CanEqual:
  override def typ: Sort = sort
  def l: Int
  def lift(l: Int): Sort

object Sort:
  def pi(a: Sort, b: Sort): Sort =
    b match
    case Prop(l) => Prop(a.l max l)
    case Set(l) => Set(a.l max l)
  
  def sigma(ss: Seq[Sort]): Sort =
    ss.foldLeft(Prop.Zero : Sort) { (a, b) => lub(a, b) }
  
  def ind(ss: Seq[Sort]): Sort = if ss.size == 0 then Prop.Zero else Set(ss.map(_.l).max)

  def lub(a: Sort, b: Sort): Sort =
    (a, b) match
    case (Prop(x), Prop(y)) => Prop(x max y)
    case _ => Set(a.l max b.l)

  def subsort(s1: Sort, s2: Sort, dir: Int): Boolean =
    (s1, s2) match
    case (Prop(a), Prop(b)) => a == b || sameSide(a - b, dir)
    case (Set(a), Set(b)) => a == b || sameSide(a - b, dir)
    case _ => false
    // if s1 == s2 then true else
    //   (s1, s2) match
    //   case (Prop(a), Prop(b)) => sameSide(a - b, dir)
    //   case (a, Set(b)) if dir < 0 => a.l == b || sameSide(a.l - b, dir) // FIXME is this correct?
    //   case (Set(a), b) if dir > 0 => a == b.l || sameSide(a - b.l, dir)
    //   case _ => false

case class Prop(val l: Int) extends Sort:
  override def sort = Set(l)
  def lift(u: Int): Prop = Prop(l + u)
object Prop:
  val Zero = Prop(0)

case class Set(val l: Int) extends Sort:
  override def sort = Set(l + 1)
  def lift(u: Int): Set = Set(l + u)
object Set:
  val Zero = Set(0)
