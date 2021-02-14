package mlang.elaboration



val builtIns = """
def ⊥ : prop := enum { }
def ⊤: prop := record { }
def refl: (#A: set, #a: A) => a = a := axiom
def sym: (#A: set, #a #b: A, eq: a = b) => b = a := axiom
def trans: (#A: set, #a #b #c: A, eq1: a = b, eq2: b = c) => a = c := axiom
def ap: (#A: set, #B: set, f: A => B, #x #y: A, eq: x = y) => f(x) = f(y) := axiom
def coe: (#A #B: set, p: A = B, a: A) => B := axiom
def iff: (A: prop, B: prop) => prop := { A, B -> record (lir: A => B, ril: B => A) }
""".getBytes().nn
