package ast

case class Skip(sk : String) extends  Prog{
  override def toString: String = "skip "
}
