package ast

case class Set(x : Variable, exp : Expr) extends Prog {
  override def toString: String = x + ":="+exp
}
