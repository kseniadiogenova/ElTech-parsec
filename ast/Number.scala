package ast

case class Number (value : Int) extends Expr{
  override def toString: String = "num : "+ value
}
