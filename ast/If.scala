package ast

/**
  * Created by kazac on 11.12.2016.
  */
case class If(condition : Expr, progl : Prog, progr : Prog) extends  Prog{
  override def toString: String = "if "+condition + " then "+ progl + "else" + progr
}
