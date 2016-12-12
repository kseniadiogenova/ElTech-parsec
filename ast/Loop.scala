package ast

case class Loop (condition : Expr, prog : Prog)extends  Prog{
  override def toString: String = "while " + condition.toString + "do" + prog.toString + "od"
}
