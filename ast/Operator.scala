package ast

case class Operator (val el : Expr,val er : Expr,val op : String) extends Expr{
  override def toString: String = el.toString +  op.toString + er.toString
}
