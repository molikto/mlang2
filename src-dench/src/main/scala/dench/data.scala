package dench

import utils._

case class FatToken(typ: TokenId, str: String, attachments: Map[String, String]):
  contract { str.nonEmpty || attachments.nonEmpty }
type Token = TokenId | FatToken

case class Document(magic: Long, content: Seq[Token])