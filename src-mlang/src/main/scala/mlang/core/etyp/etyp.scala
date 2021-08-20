package mlang.core.etyp

import utils._
import mlang.core._

/**
  * etyp are part of the typs that
  * 1. only relevant to eleborator
  * 2. have relatively trivial decideable equality
  * so in conversion checking etyps are compared by ==
  */
sealed trait Etyp

type Field = Plicit[Name]

case class Record(fields: Seq[Field]) extends Etyp:
  def apply(name: Name): (Int, Field) | Null = 
    val index = fields.indexWhere(k => k.unwrap == name)
    if index >= 0 then
      (index, fields(index))
    else null
  def ~ (other: Record): Boolean = this.eq(other) || fields == other.fields

case class Kase(name: Name, fields: Boolean):
  def ~ (other: Kase) = name == other.name && fields == other.fields
 // fields.size == other.fields.size && fields.zip(other.fields).forall(p => p._1.equalsType(p._2))

case class Enum(kases: Seq[Kase]) extends Etyp:
  def apply(i: Int): Kase = kases(i)
  def apply(name: Name): (Int, Kase) | Null = 
    val index = kases.indexWhere(k => k.name == name)
    if index >= 0 then
      (index, kases(index))
    else null
  def ~ (other: Enum) = this.eq(other) || kases.size == other.kases.size && kases.zip(other.kases).forall(p => p._1 ~ p._2)

case class Pi(name: Plicit[Name]) extends Etyp:
  override def toString(): String = name.toString()
  def ~ (other: Pi) = name.plicity == other.name.plicity
