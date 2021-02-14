package dench.bootstrap

import dench._
import dench.helper._
import scala.util.matching.Regex
import dench.Parser._


private val builder = LanguageBuilderStateMachine()
import builder._

def plicitWrapped(node: NodeDef): Seq[NodeDef] = {
  val name = node.name
  Seq(node,
    s"Implicit${name}_w" ~> seq("#", "inner"~name),
    s"Implicit$name" ~> mapped(s"Implicit${name}_w", external(s"Plicit[$name]"), s"unwrap_Implicit_$name", 
      s"def unwrap_Implicit_$name(t: Implicit${name}_w): Plicit[$name] = Plicit(t.inner, Plicity.Im)"
    ),
    s"InstanceImplicit${name}_w" ~> seq("$", "inner"~name),
    s"InstanceImplicit$name" ~> mapped(s"InstanceImplicit${name}_w", external(s"Plicit[$name]"), s"unwrap_InstanceImplicit_$name", 
      s"def unwrap_InstanceImplicit_$name(t: InstanceImplicit${name}_w): Plicit[$name] = Plicit(t.inner, Plicity.Instance)"
    ),
    s"Explicit$name" ~> mapped(name, external(s"Plicit[${name}]"), s"wrap_Explicit_$name",
      s"implicit def wrap_Explicit_$name(t: $name): Plicit[$name] = Plicit(t, Plicity.Ex)"
    ),
    s"Plicit$name" ~> choice(s"Explicit$name", s"Implicit$name", s"InstanceImplicit$name"),
 )
}

def mappedInfix(assoc: Assoc, p: Int, extension: String, op: String): Seq[NodeDef] = {
  Seq(
    s"Raw_$extension" ~> prec(assoc, p, seq("left"~"Term", op, "right"~"Term")),
    s"Op_$extension" ~> mapped(s"Raw_$extension", external("Term"), s"convert_$extension", 
      s"""def convert_$extension(t: Raw_$extension): Term = App(Proj(t.left, "${extension}"), Seq(t.right))"""
    )
  )
}

def mappedPrefix(assoc: Assoc, p: Int, extension: String, op: String): Seq[NodeDef] = {
  Seq(
    s"Raw_$extension" ~> prec(assoc, p, seq(op, "right"~"Term")),
    s"Op_$extension" ~> mapped(s"Raw_$extension", external("Term"), s"convert_$extension", 
      s"""def convert_$extension(t: Raw_$extension): Term = App(Proj(t.right, "${extension}"), Seq())"""
    )
  )
}


private val mlang = Language(0, "mlang-bootstrap", 1,
  Seq(
    "(", // delimiter pairs
    ")",
    "{",
    "}",

    ";", // seps
    "|",
    ",",

    ":", // infix seps
    "=>",
    "->",
    ":=",
    "@",

    // prefixes
    "#",
    "$",

    "^", // infixes seps, tight
    ".",

    // argumably these should all be user defined
    "=", // infix ops, loose
    "==", // tighter
    "+",
    "-",
    "*",
    "/",
    "%",
    "&&",
    "||",
    "!",

    "record ",
    "field ",
    "enum ",
    "partial ",
    "import ",
    "namespace ",
    "under ",
    "let ",
    "match ",
    "\\",
    TokenDef(token("Nat"), TokenTyp.Named("Nat", Regex("""[0-9]+"""), "Int")), // fat tokens

    constant("Hole", "_"),
    constant("BoxTypeDef", "def "),
    constant("BoxTypeClass", "class "),
    constant("BoxTypeExtension", "extension "),
    constant("BoxTypeInstance", "instance "),
    TokenDef(token("NonEmptyName"), TokenTyp.Named("NonEmptyName", Regex("""([^\x00-\x1F\s0-9:;`"'@#.,|^&<=>+\-*/\\%?!~()\[\]{}\u0024\uFEFF\u2060\u200B\u00A0]|\\u[0-9a-fA-F]{4}|\\u\{[0-9a-fA-F]+\})([^\x00-\x1F\s:;`"'@#.,|^&<=>+\-*/\\%?!~()\[\]{}\uFEFF\u2060\u200B\u00A0]|\\u[0-9a-fA-F]{4}|\\u\{[0-9a-fA-F]+\})*"""), "String")) // js regex modified to exclude $
  ),
  unwrapNodeDefs(
    "SourceFile" ~> seq("imports"~rep("Import"), "boxes"~rep("Box")),
    // the parser enforces what can be empty or not, the code inside doesn't care (without dependent type)
    plicitWrapped(
      "Name" ~> choice("NonEmptyName", mapped("_", external("NonEmptyName"), "mapped_Hole_Name"))
    ),

    "RefSeq" ~> prec(Assoc.Left, 1000, seq("names"~repsep1("NonEmptyName", ";"))), // why there needs a assoc?
    "Ref" ~> mapped("RefSeq", external("Ref"), "map_Ref"),
    "LiftRef" ~> prec(Assoc.Left, 300, seq("head"~"Ref", "^", "up"~"Nat")),

    "ImportAll" ~> mapped("_", external("Unit"), "mapped_Hole_Unit"), // TODO this syntax results in AST is have too much necessary nesting...
    "ImportGrouped" ~> seq("{", "items"~repsep("ImportExpr", ","), "}"),
    "ImportQuantified" ~> seq("head"~"Ref", ";", "tail"~"ImportExpr"),
    "ImportExpr" ~> choice("Ref", "ImportQuantified"),
    "Import" ~> mapped(seq("import ", "ImportExpr"), "ImportExpr"),

    "NamesTyp" ~> seq("names"~rep("PlicitName"), ":", "typ"~"Term"),
    "BoxType" ~> choice("def ", "extension ", "instance ", "class "),
    "Def" ~> seq(opt("partial "), "keyword"~"BoxType", "name"~"Name", "intros"~opt("Tele"), "typ"~opt(seq(":", "Term")), ":=", "body"~"Term"),
    "Field" ~> mapped(seq("field ", "NamesTyp"), "NamesTyp"),
    "Under" ~> seq("under ", "intros"~"Tele", "{", "boxes"~rep("Box"), "}"),
    "Namespace" ~> seq("namespace ", "ref"~"Ref", "{", "boxes"~rep("Box"), "}"),
    "Box" ~> choice("Def", "Under", "Namespace"),
    "BoxOrField" ~> choice("Box", "Field"),

    "MakePattern" ~> seq("(", "fields"~repsep("PlicitPattern", ","), ")"),
    "ConstructPattern" ~> seq("kase"~"NonEmptyName", "(", "fields"~repsep("PlicitPattern", ","), ")"),
    plicitWrapped("Pattern" ~> choice("Name", "MakePattern", "ConstructPattern")),

    "Tele" ~> alternative(seq("{", rep("BoxOrField"),"}"), seq("(", repsep("NamesTyp", ","),")")),
    "Tele_w" ~> prec(Assoc.None, 1, seq("inner"~"Tele")),
    "Term_w" ~> seq("inner"~"Term"),
    "TeleOrTerm" ~> choice(mapped("Term_w", "Tele", "unwrap_Term_Tele"), mapped("Tele_w", "Tele", "unwrap_Tele")),

    "App" ~> prec(Assoc.None, 100, seq("head"~"Term", "(", "args"~repsep("PlicitTerm", ","), ")")),
    "ProjRight" ~> choice("Ref", "LiftRef"),
    "Proj" ~> prec(Assoc.None, 200, seq("head"~"Term", ".", "field"~"ProjRight")),
    "Kase" ~> seq("name"~"Name", "fields"~opt("Tele")),
    "Enum" ~> seq("enum ", "{", "kases"~repsep("Kase", "|"), "}"),
    "Let" ~> seq("let ", "{", "items"~rep("BoxOrTerm"), "}"),
    "Eq" ~> prec(Assoc.Right, 3, seq("left"~"Term", "=", "right"~"Term")),
    "Branch" ~> seq("params"~repsep1("PlicitPattern", ","), "->", "clos"~"Term"),
    "Lambda" ~> prec(Assoc.Right, 1, seq("{", "branches"~repsep("Branch", "|"), "}")),
    "Pi" ~> prec(Assoc.Right, 2, seq("dom"~"TeleOrTerm", "=>", "cod"~"Term")), // THis semes not working, not right assoc now
    "At" ~> prec(Assoc.Left, 4, seq("left"~"Term", "@", "right"~"Term")),
    "Match" ~> seq("match ", "terms"~repsep1("Term", ","), "lam"~"Lambda"),
    "Record" ~> seq("record ", "fields"~"Tele"),
    // https://github.com/agda/agda-stdlib/blob/e34a31f80b215812ab26c10f84c9a658eeda3110/README/Design/Fixity.agda#L14
    // https://github.com/tree-sitter/tree-sitter-javascript/blob/master/grammar.js#L738
    mappedInfix(Assoc.Left, 20, "or", "||"),
    mappedInfix(Assoc.Left, 30, "and", "&&"),
    //mappedInfix(Assoc.Left, 40, "equals", "=="),
    mappedInfix(Assoc.Left, 50, "plus", "+"),
    mappedInfix(Assoc.Left, 50, "minus", "-"),
    mappedInfix(Assoc.Left, 60, "times", "*"),
    mappedInfix(Assoc.Left, 60, "divides", "/"),
    mappedInfix(Assoc.Left, 60, "modulo", "%"),
    mappedPrefix(Assoc.Left, 90, "not", "!"),
    plicitWrapped("Term" ~> choice(
      "Op_or",
      "Op_and",
    //  "Op_equals",
      "Op_plus",
      "Op_minus",
      "Op_times",
      "Op_divides",
      "Op_modulo",
      "Op_not",
      "Enum", "Ref", "Eq", "Lambda", "App", "At", "Match", "LiftRef", "Proj", "Pi", "Record","Let", "_", "Nat", seq("(", "Term", ")"))),
    "BoxOrTerm" ~> choice("Box", "Term"),
  ),
)


@main def bootstrap() = DenchGen(mlang)
