package dench

import utils._


enum Typ:
  case External(typ: String)
  case Constant(name: TokenId)
  case Ref(name: NodeId)
  case Opt(typ: Typ)
  case Seq(typ: Typ)
  case Mapped(org: Typ, to: Typ, mapper: String | Null)
  case Choice(kases: scala.collection.Seq[Typ])
  case Product(fields: scala.collection.Seq[Typ])
  case Field(name: String, typ: Typ)

extension (t: Seq[Typ]) def single: Typ =
  if t.size == 0 then
    notImplemented()
  else if t.size == 1 then
    t(0) match
    case _: Typ.Field => Typ.Product(t) // don't unwrap in case the field is named
    case _ => t(0)
  else 
    Typ.Product(t)