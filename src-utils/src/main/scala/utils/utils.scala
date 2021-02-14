package utils

def sameSide(a: Int, b: Int) = (a > 0 && b > 0) || (a < 0 && b < 0)

extension [T] (t: T | Null) def nn: T = if t == null then logicError() else t