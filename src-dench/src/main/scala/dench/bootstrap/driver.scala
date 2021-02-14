package dench.bootstrap

import utils._
import treesitter.TreeSitterNative._

private var bytes: Array[Byte] = Array()

def parse(str: Array[Byte]): SourceFile =
  val parser = ts_parser_new()
  ts_parser_set_language(parser, tree_sitter_dench_generated())
  bytes = str
  val string = new String(str)
  val tree = ts_parser_parse_string(parser, null, string, str.size)
  val cursor = ts_tree_cursor_new(ts_tree_root_node(tree)).nn
  Parsers.SourceFile_parser(cursor)

def ts_tree_cursor_goto_first_named_child(node: TSTreeCursor): Boolean =
  var b = ts_tree_cursor_goto_first_child(node)
  if b && ts_node_is_named(ts_tree_cursor_current_node(node)) then true
  else if !b then false
  else if ts_tree_cursor_goto_next_named_sibling(node) then
    true
  else
    ts_tree_cursor_goto_parent(node)
    false

def String_parser(node: TSTreeCursor): String =
  val c = ts_tree_cursor_current_node(node)
  val a = ts_node_start_byte(c)
  val b = ts_node_end_byte(c)
  new String(bytes.slice(a, b))

def Nat_parser(node: TSTreeCursor): Int =
  val c = ts_tree_cursor_current_node(node)
  val a = ts_node_start_byte(c)
  val b = ts_node_end_byte(c)
  new String(bytes.slice(a, b)).toInt

def handleUnknown(a: Int, node: TSTreeCursor): Nothing =
  throw Exception(s"unknown case $a, with text: ${String_parser(node)}")

def ts_tree_cursor_goto_next_named_sibling(node: TSTreeCursor): Boolean =
  var b = ts_tree_cursor_goto_next_sibling(node)
  if b && ts_node_is_named(ts_tree_cursor_current_node(node)) then true
  else if !b then false
  else ts_tree_cursor_goto_next_named_sibling(node)