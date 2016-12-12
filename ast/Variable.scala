package ast

case class Variable(name : String) extends Expr{
  override def toString: String = "var: "+name
}
