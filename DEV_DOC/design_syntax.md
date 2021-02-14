# On programming language syntax

My thesis is that popular programming language syntax 
- is designed to be convenient for code writers, not readers
- alternative design that are both convenient for writer & readers can be achieved by better tooling

I am using the word "pl syntax workbench" here. it is not language workbench, but only syntax.

## why?

* approach: rich token stream
  * rich text presentation of syntax. fonts, tables, non-linear layouting of mathematical formulas, hidden marks (delimiter insersion)
  * semantically directed editing. this ofc can be done with traditional editors (?)
    * we want more: we think some desicion natually found it's place in editing phase
  * how to extend parser? is it possible to decide if a parsing rule is conservative with the other?, adding a new op/keyword should always be, this is why rich tokens is good~
* approach: rich token tree (tree structure of tokens, without tags)
* approach: concrete syntax tree (with tags)
* approach: abstract syntax tree

## how

* ui
  * data layer
  * logic layer
  * layout layer
  * presentation layer
## camelCase vs snake_case

`I_don't_want_to_elaborate_why_sanke_case_is_more_readable thanCamelCaseBecauseIDontHaveTheTimeButLetAssumeThis`. But why people use `camelCase`?

- It is writer friendly, 1 less key to press (`_`)
- it actually aids readers, because in `arg: Int`, it is obvious `Int` is a type. it can be solved by using a highlighting for typeable expressions.
- less name collesions. `type: Type`. in snake case naming one might needs to choice to write `t: type` or `T: type`. I don't have a good solution to this...

## match expr vs pattern lambda

- pattern lambda
  - no extra variable is re-introduced
  - the return type etc is elaborated by the case, but in match expr they are not. this is why. so in Lean if you want some proof without tatics, you still use pattern lambda, but in Coq you use tatics all the time...
- match expression
  - good: more flexable, it is used to introduce `a` from `a, b, c` without introducing `b, c`

so what I have choose
  - surfuace: the Lean way, have both pattern and match
  - core: the Agda way, have pattern lambda

## C application syntax `f(x, y(z))` or Haskell `f x (y z)`

- `def ONE : term := lam(lam(app(var(1), var(0))))`
- `def ONE : term := lam (lam (app (var 1) (var 0)))`

the main good thing about first notation is:
- in situations like `a.f(a, b).g(c, d)` it is **much better** than `((a.f a b).g c d)`
- uniform, or say, less `()`. so editor friendly. you can argue that Haskell way is also uniform, but the result is the same: in the first situation, you make less decision about "should I insert a `(`?" **before** you write what's after the `(`


## `package_name.type_name.constructor_name.extension_function` or `p:t:c.e`?

- use second because all the decisions above (`snake_case` all the place) makes first convension hard to parse (without highlighting?)
- see bellow

## constructors

we **don't want to put constructor names at the same name namespace with the type**. in Haskell, you are forced to use ugly constructor names 

so we consider what happens when we want to fit constructor namespacing in the convension `a.b` or `a:b`:
- first case
  - `nil`, `cons(a, b)` is ok when checking ✅
  - `list.nil` doesn't makes sense
  - `list.cons(a, b)` doesn't makes sense
  - `list(nat).nil` feels natual ✅
  - `list(nat).cons(a, b)` you don't want to write the type args, but you must
- second case
  - `nil`, `cons(a, b)` is ok when checking ✅
  - `list:nil` cannot be inferred
  - `list:cons(a, b)` is inferable ✅
  - `list:nil(#nat)` is ugly
  - `list:cons(a, b)` is natual! ✅

so in the end, the second one is better. also it is a natual extension of namespacing, where the first one feels hacky, it is an extra elaboration step, or a very unnatual way to attach extension methods to objects instead of types
  - ~~another way is introduce a `constuct` syntax. `nil@list(nat)`, `list(nat)@cons(a, b)` it is like `make(a, b)`~~
  - just use `the(list(nat), nil)`. this is natual to understand as that our constructor syntax is always **untyped** just like the core, and this means they must be checked instead of inferred. this actually is natual

so in conclution:

- when checked, we are good `nil`
- when infering, if type args can be infered, we are good `list:cons(a, b)`
- when not, the library construct can make it as checked expr! `the(list(nat), nil)`

## first principle

- I think we are not getting rid of the parser. Grammars are essential for compactly represent the programs, and they define patterns, and human mind are great at pattern recog
- we can get rid of lexers

