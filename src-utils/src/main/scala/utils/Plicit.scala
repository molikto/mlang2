package utils


enum Plicity:
  case Ex, Im, Instance

extension (p: Plicity) def impli: Boolean = p != Plicity.Ex

case class Plicit[T](unwrap: T, plicity: Plicity)
