package dench.bootstrap

import dench._
import utils._
import scala.collection.mutable

class DenchGen(lan: Language):
  val moduleFolder= os.pwd/"modules"/"tree-sitter-javacpp"
  val folder = moduleFolder/"grammar"
  os.write.over(folder/"grammar.js", GrammarJsGen(lan).result, createFolders = true)
  if !os.exists(folder/"node_modules"/"tree-sitter-cli") then
    os.proc("npm", "install", "tree-sitter-cli").call(cwd = folder)
  os.proc("./node_modules/.bin/tree-sitter", "generate").call(cwd = folder)
  os.proc("bash", "./gen.sh").call(cwd = moduleFolder)
  val parser_c = os.read.lines(folder/"src"/"parser.c").toSeq
  os.write.over(os.pwd/"src-dench"/"src"/"main"/"scala"/"dench"/"bootstrap"/"generated.scala", ScalaTypsGen(lan, parser_c).result, createFolders = true)

class ScalaTypsGen(lan: Language, parser_c: Seq[String]):
  def mapAnno(str: String): String =
    if str.exists(_.isLower) then str
    else 
      str.split("_").asInstanceOf[Array[String]].map(_ match {
        case "COLON" => ":"
        case "EQ" => "="
        case "LPAREN" => "("
        case "RPAREN" => ")"
        case "COMMA" => ","
        case "LBRACE" => "{"
        case "POUND" => "#"
        case "AMP" => "&"
        case "SEMI" => ";"
        case "RBRACE" => "}"
        case "DOT" => "."
        case "PLUS" => "+"
        case "STAR" => "*"
        case "DASH" => "-"
        case "SLASH" => "/"
        case "PERCENT" => "%"
        case "BANG" => "!"
        case "CARET" => "^"
        case "DOLLAR" => "$"
        case "AT" => "@"
        case "GT" => ">"
        case "PIPE" => "|"
        case "BSLASH" => "\\"
      }).mkString("")
  val symsStrs = parser_c.filter(_.startsWith("  sym_")).map(line => {
    val s: Array[String] = line.drop("  sym_".size).dropRight(1).split(" = ").asInstanceOf[Array[String]]
    (s(0), s(1).toInt)
  }).filter(!_._1.startsWith("_")).filter(_._1 != "source_file").filter(_._1 != "comment")
  val annoStrs = parser_c.filter(_.startsWith("  anon_sym_")).map(line => {
    val s: Array[String] = line.drop("  anon_sym_".size).dropRight(1).split(" = ").asInstanceOf[Array[String]]
    (mapAnno(s(0)), s(1).toInt)
  })
  val symbolMap = (symsStrs ++ annoStrs).map(a => (lan.reverse(a._1), a._2)).toMap
  val fieldMap = parser_c.filter(_.startsWith("  field_")).map(line => {
    val s: Array[String] = line.drop("  field_".size).dropRight(1).split(" = ").asInstanceOf[Array[String]]
    (s(0), s(1).toInt)
  }).toMap
  def gen(typ: Typ, ignore: String | Null = null): String =
    lan.demappedRight(typ) match
    case Typ.Constant(r) => 
      lan(r).typ match
      case TokenTyp.Named(_, _, typ) => typ
      case TokenTyp.Constant(name, _) => name + ".type"
      case TokenTyp.Unnamed(_) => notImplemented()
    case Typ.Ref(r) =>
      val df = lan(r)
      if df.ptyp == ParserTyp.None then gen(lan.typs(r)) else df.name
    case Typ.Opt(typ) => s"${gen(typ)} | Null"
    case Typ.Seq(typ) => s"Seq[${gen(typ)}]"
    case Typ.Choice(kases) => kases.map(a => gen(a)).filter(a => a != ignore).mkString(" | ")
    case Typ.External(a)=> a
    case a => 
      notImplemented()


  def gen(node: NodeDef, typ: Typ): String =
    val name = node.name
    typ match
    case Typ.Product(fs) if fs.forall(f => f.isInstanceOf[Typ.Field]) =>
      val ns = fs.map(_.asInstanceOf[Typ.Field])
      s"case class $name(${ns.map(n => n.name + ": " + gen(n.typ)).mkString(", ")})"
    case _ =>
      val inner = gen(typ, name)
      if inner != "" then s"type ${name} = ${inner}" else ""

  def genParser(typ: Typ): String =
    typ match
    case Typ.Mapped(a, b, m) => if m == null then genParser(a) else m + "(" + genParser(a) + ")"
    case Typ.Constant(r) => lan(r).name + "_parser(node)"
    case Typ.Ref(r) =>
      if lan(r).ptyp == ParserTyp.None then
        genParser(lan.typs(r))
      else
        lan(r).name + "_parser(node)"
    case Typ.Choice(kases) =>
      val done = mutable.Set.empty[Typ]
      def rec(ks: scala.collection.Seq[Typ]): scala.collection.Seq[String] = {
        ks.filter(a => !done.contains(a)).map(a => { done += a; a }).flatMap(tp => lan.demappedLeft(tp) match {
          case Typ.Choice(kases) =>
            rec(kases)
          case Typ.Constant(a) => 
            Seq(s"case ${symbolMap(a)} => ${genParser(tp)}")
          case Typ.Ref(a) =>
            lan.demappedLeft(lan.typs(a)) match {
              case Typ.Choice(bs) => rec(bs)
              case _ =>
                Seq(s"case ${symbolMap(a)} => ${genParser(tp)}")
            }
          case a => notImplemented()
        })
      }
      val ls = rec(kases)
      s"{ val sym = ts_node_symbol(ts_tree_cursor_current_node(node)); sym.toInt match { ${ls.mkString("; ")}; case a => handleUnknown(a, node) } }"
    case Typ.Seq(typ@Typ.Ref(_)) =>
      val declares = s"val _vs: mutable.ArrayBuffer[${gen(typ)}] = new mutable.ArrayBuffer()"
      val cases= s"_vs += ${gen(typ)}_parser(node)"
      val loop = s"while (_cont) { $cases; _cont = ts_tree_cursor_goto_next_sibling(node) }" // LATER don't support field ids
      s"{ val _cont0 = ts_tree_cursor_goto_first_named_child(node); var _cont = _cont0; $declares; $loop; if (_cont0) ts_tree_cursor_goto_parent(node); _vs.toSeq }"
    case a =>
      notImplemented()
  
  def genParser(node: NodeDef, typ: Typ): String =
    val name = node.name
    typ match
    case Typ.Product(fs) if fs.forall(f => f.isInstanceOf[Typ.Field]) =>
      val ns = fs.map(_.asInstanceOf[Typ.Field])
      val declares = ns.map(a => {
        def rec(typ: Typ): String = {
          lan.demappedRight(typ) match {
          case Typ.Opt(typ) => s"var ${a.name}: ${gen(typ)} | Null = null"
          case Typ.Seq(typ) => s"val ${a.name}: mutable.ArrayBuffer[${gen(typ)}] = new mutable.ArrayBuffer()"
          case typ => s"var ${a.name}: ${gen(typ)} | Null = null"
          }
        }
        rec(a.typ)
      }).mkString("; ")
      val cases= ns.map(a => {
        def rec(t: Typ): String = {
          t match {
            case Typ.Mapped(a, b, m) => if m == null then rec(a) else m + "(" + rec(a) + ")"
            case Typ.Opt(typ) => rec(typ)
            case Typ.Seq(typ) => rec(typ)
            case Typ.Ref(a) =>
              lan.typs(a) match
              case m: Typ.Mapped => rec(m)
              case _ => genParser(t)
            case Typ.Constant(a) => genParser(t)
            case a => notImplemented()
          }
        }
        a.typ match {
        case Typ.Opt(typ) => s"case ${fieldMap(a.name)} => ${a.name} = ${rec(a.typ)}"
        case Typ.Seq(typ) => s"case ${fieldMap(a.name)} => ${a.name} += ${rec(a.typ)}"
        case typ => s"case ${fieldMap(a.name)} => ${a.name} = ${rec(a.typ)}"
        }
      }).mkString("; ")
      val loop = s"while (_cont) { ts_tree_cursor_current_field_id(node).toInt match { $cases; case a => handleUnknown(a, node) }; _cont = ts_tree_cursor_goto_next_named_sibling(node) }"
      val gets = ns.map(a => {
        lan.demappedRight(a.typ) match {
        case Typ.Opt(typ) => a.name
        case Typ.Seq(typ) => a.name + ".toSeq"
        case typ => s"${a.name}.nn.asInstanceOf[${gen(typ)}]"
        }
      }).mkString(", ")
      s"def ${name}_parser(node: TSTreeCursor): ${name} = { val _cont0 = ts_tree_cursor_goto_first_named_child(node); var _cont = _cont0; $declares; $loop; if (_cont0) ts_tree_cursor_goto_parent(node); ${name}($gets) }"
    case _ =>
      s"def ${name}_parser(node: TSTreeCursor): ${name} = ${genParser(typ)}"

      
  val pairs = lan.typs.map(a => (lan(a._1), a._2)).filter(a => a._1.ptyp != ParserTyp.None)
  val parserPairs = pairs.filter(a => a._1.ptyp != ParserTyp.Unwrap)
  val constants = lan.tokens.filter(_.typ.isInstanceOf[TokenTyp.Constant]).map(a => s"object ${a.name}; def ${a.name}_parser(node: TSTreeCursor): ${a.name}.type = ${a.name}").mkString("\n")
  val result = "package dench.bootstrap\nimport utils._\nimport scala.collection.mutable\nimport treesitter.TreeSitterNative._\n" + lan.codes.mkString("\n")  + "\n" + constants + "\n" + pairs.map(a => gen(a._1, a._2)).mkString("\n") + "\nobject Parsers {\n"+ parserPairs.map(a => genParser(a._1, a._2)).mkString("\n") + "\n}"

class GrammarJsGen(lan: Language):
  // def escape(str: String): String = 
  //   val res = str.replace("\"", "\\\"")
  //   if (res == null) notImplemented() else res

  val plain = true

  //def name(t: TokenDef): String = escape(t.name)
  def name(t: NodeDef): String = 
    if (t.id == lan.root) then "source_file"
    else (if (t.ptyp != ParserTyp.Normal) "_" else "") + t.name
    // "\"" + (if (t.unwrap) "_" else "")  + t.name + "\""
    
  def ref(p: TokenId): String = 
    if plain then
      lan(p).typ match
      case TokenTyp.Unnamed(name) => "\"" + name.replace("\\", "\\\\").replace("\"", "\\\"") + "\""
      case TokenTyp.Constant(name, typ) => "_." + name
      case TokenTyp.Named(name, _, _)=> "_." + name
    else s"'\\U+${p.get.toHexString}'"

  def gen(p: TokenId): String =
    lan(p).typ match
    case TokenTyp.Unnamed(lit) => notImplemented()
    case TokenTyp.Named(_, regex, _) => s"/${regex.toString}/"
    case TokenTyp.Constant(_, lit) => "\"" + lit + "\""

  def gen(p: Parser): String =
    p match
    case Parser.choice(bs @ _*) => s"choice(${bs.map(gen).mkString(", ")})"
    case Parser.alternative(bs @ _*) => s"choice(${bs.map(gen).mkString(", ")})"
    case Parser.external(_) => logicError("external parser not allowed, it is only used to mark type of a mapped parser")
    case Parser.seq(cs @ _*) => s"seq(${cs.map(gen).mkString(", ")})"
    case Parser.rep(c) => s"repeat(${gen(c)})"
    case Parser.rep1(c) => s"repeat1(${gen(c)})"
    case Parser.rep2(c) => s"repeat2(${gen(c)})"
    case Parser.repsep(c, b) => s"repsep(${gen(c)}, ${ref(b)})"
    case Parser.repsep1(c, b) => s"repsep1(${gen(c)}, ${ref(b)})"
    case Parser.opt(c) => s"optional(${gen(c)})"
    case Parser.field(name, p) => s"field('${name}', ${gen(p)})"
    case Parser.mapped(a, _, _, _) => gen(a)
    case Parser.prec(assoc, p, c) =>
      val as = assoc match
      case Assoc.None => ""
      case Assoc.Left => ".left"
      case Assoc.Right => ".right"
      s"prec$as($p, ${gen(c)})"
    case Parser.Ref(t) => 
      t match
      case n: NodeId => 
        if lan(n).ptyp == ParserTyp.None then
          gen(lan(n).parser)
        else
          "_." + name(lan(n))
      case n: TokenId =>
        ref(n)

  val tokens = lan.tokens.filter(!_.typ.isInstanceOf[TokenTyp.Unnamed]).map(t => s"""    ${t.name}: _ => ${gen(t.id)},""").mkString("\n")
  val nodes = lan.nodes.filter(a => a.ptyp != ParserTyp.None).map(t => s"""    ${name(t)}: _ => ${gen(t.parser)},""").mkString("\n")

  val result = """
  |const repsep1 = (rule, comma) => {
  |  return seq(rule, repeat(seq(comma, rule)))
  |}
  |const repsep = (rule, comma) => {
  |  return optional(repsep1(rule, comma))
  |}
  |const repeat2 = (rule) => {
  |  return seq(rule, repeat1(rule))
  |}
  |module.exports = grammar({
  |  name: 'dench_generated',
  |  extras: _ => [
  |    _._comment,
  |    /[\s\uFEFF\u2060\u200B\u00A0]/
  |   ], // copied from JavaScript parser
  |  rules: {
  |   """.stripMargin + s"""
  |$nodes
  |$tokens
  |""".stripMargin + """
  |    _comment: _ => token(prec(1, choice(
  |      seq('//', /.*/),
  |      seq(
  |        '/*',
  |        /[^*]*\*+([^/*][^*]*\*+)*/,
  |        '/'
  |      )
  |    ))),
  |  }
  |});
  |""".stripMargin