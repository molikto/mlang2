package dench

import scala.util.matching.Regex
import scala.collection.mutable
import utils._

case class TokenId(get: Int)
case class NodeId(get: Int)

type NodeOrTokenId = NodeId | TokenId

enum Assoc:
  case None, Left, Right
enum Parser:
  case alternative(branches: Parser*)
  case choice(branches: Parser*)
  case seq(children: Parser*)
  case rep(children: Parser)
  case rep1(children: Parser)
  case rep2(children: Parser)
  case mapped(
    body: Parser,
    resultTyp: Parser,
    name: String | Null = null,
    code: String | Null = null)
  case repsep(children: Parser, sep: TokenId)
  case repsep1(children: Parser, sep: TokenId)
  case opt(children: Parser)
  case prec(assoc: Assoc, p: Int, child: Parser)
  case field(name: String, parser: Parser)
  case external(typ: String) // only used by mapper
  case Ref(typ: NodeOrTokenId)

def recPtyp(p: Parser): ParserTyp = 
  p match
  case _: Parser.choice => ParserTyp.Trait
  case _: Parser.alternative => ParserTyp.Unwrap
  case _: Parser.mapped => ParserTyp.None
  case Parser.prec(_, _, a) => recPtyp(a)
  case _ => ParserTyp.Normal

enum TokenTyp:
  case Unnamed(literal: String)
  case Constant(name: String, literal: String)
  case Named(name: String, regex: Regex, typ: String)

case class TokenDef(
  id: TokenId,
  typ: TokenTyp
): 
  val name =
    typ match
    case TokenTyp.Unnamed(lit) => lit
    case TokenTyp.Constant(n, _) => n
    case TokenTyp.Named(n, _, _) => n

enum ParserTyp:
  case Normal
  case Trait
  case Unwrap
  case None

case class NodeDef(id: NodeId, name: String, parser: Parser):
  val ptyp = recPtyp(parser)


case class Language(
  denchv: Int, // dench version
  name: String,
  version: Int,
  tokens: Seq[TokenDef],
  nodes: Seq[NodeDef],
  //conflicts: Seq[Seq[NodeDef]]
):
  def apply(id: TokenId): TokenDef = tokens.find(_.id == id).get
  def apply(id: NodeId): NodeDef = nodes.find(_.id == id).get
  def name(id: NodeOrTokenId) = tokens.find(_.id == id).map(_.name).getOrElse(nodes.find(_.id == id).get)

  def reverse(str: String): NodeOrTokenId = 
    if str == "source_file" then root
    else tokens.find(_.name.trim == str).map(a => a._1: NodeOrTokenId).getOrElse(nodes.find(_.name.trim == str).get.id)

  var codes =  mutable.Set.empty[String]
  
  def named(ref: NodeOrTokenId): NodeOrTokenId | Null =
    ref match
    case t: TokenId => t
    case n: NodeId =>
      if apply(n).ptyp != ParserTyp.Normal then null
      else n
    

  // extension (p: Parser) def starts: Seq[NodeOrTokenId] =
  //   var seq = Set.empty[NodeOrTokenId]
  //   def rec(p: Parser): Unit =
  //     p match
  //     case Parser.alternative(branches @ _*) => branches.foreach(rec)
  //     case Parser.choice(branches @ _*) => branches.foreach(rec)
  //     case Parser.seq(children @ _*) => 
  //       children.head match
  //         case Parser.Ref(typ) =>
  //           val t = named(typ)
  //           if t != null then seq += t else notImplemented()
  //         case _ => notImplemented()
  //     case Parser.rep(children) => notImplemented()
  //     case Parser.rep1(children) => rec(children)
  //     case Parser.rep2(children) => rec(children)
  //     case Parser.mapped(a, _, _) => rec(a)
  //     case Parser.repsep(children, sep) => notImplemented()
  //     case Parser.repsep1(children, sep) => rec(children)
  //     case Parser.opt(children) => notImplemented()
  //     case Parser.prec(_, _, child) => rec(child)
  //     case Parser.field(name, parser) => notImplemented()
  //     case Parser.external(typ) => notImplemented()
  //     case Parser.Ref(typ) =>
  //       typ match
  //       case t: TokenId => seq += t
  //       case n: NodeId =>
  //         val nn = apply(n)
  //         val tp = nn.ptyp
  //         if tp == ParserTyp.Normal then
  //           seq += n
  //         else rec(nn.parser)
  //   rec(p)
  //   seq.toSeq

  extension (p: Parser) private def typ0(id: NodeId): Seq[Typ] =
    p match
    case Parser.choice(bs @ _*) =>
      Seq(Typ.Choice(bs.map(b => b.typ.single).filter(_ != Typ.Ref(id))))
    case a => a.typ

  extension (p: Parser) private def typ: Seq[Typ] = 
    p match
    case Parser.seq(cs @ _*) => cs.flatMap(_.typ)
    case Parser.rep(cs) => Seq(Typ.Seq(cs.typ.single))
    case Parser.rep1(cs) => Seq(Typ.Seq(cs.typ.single))
    case Parser.rep2(cs) => Seq(Typ.Seq(cs.typ.single))
    case Parser.repsep(cs, _) => Seq(Typ.Seq(cs.typ.single))
    case Parser.repsep1(cs, _) => Seq(Typ.Seq(cs.typ.single))
    case Parser.opt(c) =>
      def rec(t: Typ): Typ = 
        t match
        case a: Typ.Opt => a
        case a: Typ.Seq => a
        case a => Typ.Opt(a)
      c.typ match
      case Seq() => Seq.empty
      case Seq(a) => Seq(rec(a))
      case _ => notImplemented()
    case Parser.prec(_, _, c) => c.typ
    case Parser.mapped(l, r, m, c) =>
      if c != null then codes += c
      Seq(Typ.Mapped(l.typ.single, r.typ.single, m))
    case Parser.field(n, p) => Seq(Typ.Field(n, p.typ.single))
    case Parser.alternative(bs @ _*) => bs.head.typ
    case Parser.external(t) => Seq(Typ.External(t))
    case Parser.choice(bs @ _*) =>
      Seq(Typ.Choice(bs.map(b => b.typ.single)))
    case Parser.Ref(t) => 
      t match
      case id: NodeId =>
        val df = this(id)
        df.ptyp match
        case ParserTyp.Unwrap => df.parser.typ
        case _ => Seq(Typ.Ref(df.id))
      case id: TokenId =>
        this(id).typ match
        case TokenTyp.Unnamed(_) => Seq.empty
        case _ => Seq(Typ.Constant(id))
  
  val typs = nodes.map(n => {
    try {
      (n.id, n.parser.typ0(n.id).single)
    } catch {
      case e => 
        throw Exception(s"Failed to get typ of $n", e)
    }
  }).toMap

  def demappedRight(t: Typ): Typ =
    def rec(t: Typ): Typ | Null =
      t match
      case Typ.Ref(id) => rec(typs(id))
      case Typ.Mapped(a, b, _) => b
      case _ => null
    val res = rec(t)
    if res != null then res else t

  def demappedLeft(t: Typ): Typ =
    def rec(t: Typ): Typ | Null =
      t match
      case Typ.Ref(id) => rec(typs(id))
      case Typ.Mapped(a, b, _) => a
      case _ => null
    val res = rec(t)
    if res != null then res else t

  val root: NodeId = nodes.filter(_.name == "SourceFile")(0).id