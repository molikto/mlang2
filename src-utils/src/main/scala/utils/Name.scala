package utils

type Name = String
type Ref = Name | Seq[Name] // if a seq, must be more than one segments

extension (a: Ref) def + (o: Ref): Ref =
  val s1: Seq[Name] = a match
  case n: Name => Seq(n)
  case r: Seq[Name] => r
  val s2: Seq[Name] = o match
  case n: Name => Seq(n)
  case r: Seq[Name] => r
  val r = s1 ++ s2
  if r.size == 1 then r(0) else r

extension (r: Ref) def last: Name = 
  r match
  case n: Name => n
  case a: Seq[Name] => a.last


extension (r: Ref) def pop1: Ref =
  r match
  case n: Name => Seq.empty
  case a: Seq[Name] => a.dropRight(1)

extension (r: Ref) def pop(r2: Ref): Ref =
  var r0 = r
  var rr = r2
  while (rr.nonEmpty) {
    if r0.last == rr.last then
      r0 = r0.pop1
      rr = rr.pop1
    else logicError()
  }
  r0

extension (r: Ref) def nonEmpty: Boolean = 
  r match
  case n: Name => true
  case a: Seq[Name] => a.nonEmpty

extension (r: Ref) def isEmpty: Boolean = 
  r match
  case n: Name => false
  case a: Seq[Name] => a.isEmpty