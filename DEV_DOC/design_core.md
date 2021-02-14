# core

## integrated record & enum type like in OO languages

- tagged telescope
- unions, where items must be tagged

cons:

- a little bit remote from the litereture, theory

## iterated sigma & record

- record good & sigma bad
  - row subtyping
  - no extra "top" type
- sigma good & record bad
  - single field record type is bad?
  - `field a: ... ; def helper: ... := ... ; field b: helper(...)` can be defined by using let

- nominal fields or structural fields?
  - we can say a telescope starts with renamblabe fields... and these doesn't allow diamonds. they are kind of "id"ed. so they are kind of "nominal" fields.


## enum

- list of tele good
  - uniform syntax...?
- list of type good
  - nat.suc is not a single entry tele of nat. also in setoid type theory eq(suc(zero), suc(zero)) = top, instead of make(top). in the end the proof is make(make(make(make(top)))), very ugly...?
  - you can have seperate types for the branches, and the root "choice", but in case of recursive defined, it is semantically more natural to have it inside a single type, so you don't need to elaborate it to a mutually defined type..