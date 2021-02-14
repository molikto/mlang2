package dench.bootstrap

import utils._
import treesitter.TreeSitterNative._

def mapped_Hole_Name(s: Hole.type): Name = ""

def NonEmptyName_parser(node: TSTreeCursor): String = String_parser(node)
def map_Ref(rs: RefSeq): Ref = if rs.names.size == 1 then rs.names(0) else rs.names

def unwrap_Term_Tele(t: Term_w): Tele = Seq(NamesTyp(Seq.empty, t.inner))
def unwrap_Tele(t: Tele_w): Tele = t.inner

