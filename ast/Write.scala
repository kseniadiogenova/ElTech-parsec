package ast

case class Write(exp : Expr) extends  Prog{
  override def toString: String = "write "+ exp.toString
}
