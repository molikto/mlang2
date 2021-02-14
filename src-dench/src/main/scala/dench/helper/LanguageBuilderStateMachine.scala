package dench.helper

import dench._

import scala.collection.mutable
import scala.language.implicitConversions


class LanguageBuilderStateMachine():

  private val tokens = mutable.Map.empty[String, TokenId]
  private var tokenId = 0

  private val nodes = mutable.Map.empty[String, NodeId]
  private var nodeId = 1


  implicit def token(str: String): TokenId =
    tokens.get(str) match
    case Some(a) => a
    case _ =>
      tokenId += 2
      val res = TokenId(tokenId)
      tokens.put(str, res)
      res

  implicit def tokenDef(str: String): TokenDef = 
    TokenDef(token(str), TokenTyp.Unnamed(str))

  def constant(name: String, lit: String) = TokenDef(token(lit), TokenTyp.Constant(name, lit))
  
  implicit def node(str: String): NodeId = 
    nodes.get(str) match
    case Some(a) => a
    case _ =>
      nodeId += 2
      val res = NodeId(nodeId)
      nodes.put(str, res)
      res

  // always prefer creating a node id, because they are the only one that can be recursive!
  implicit def charToRef(str: String): Parser.Ref =
    val res: Option[NodeOrTokenId] = tokens.get(str).orElse(nodes.get(str))
    res match
    case Some(a) => Parser.Ref(a)
    case None =>
      println("warn: recursive ref or error: " + str)
      Parser.Ref(node(str))

  extension (str: String) def ~> (parser: Parser) = NodeDef(node(str), str, parser)

  extension (str: String) def ~ (parser: Parser) = Parser.field(str, parser)

  def unwrapNodeDefs(seq: (NodeDef | Seq[NodeDef])*): Seq[NodeDef] = seq.flatMap(_ match {
    case n: NodeDef => Seq(n)
    case a: Seq[NodeDef] => a
  })