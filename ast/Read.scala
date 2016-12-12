package ast
case class Read(x : Variable) extends  Prog{
  override def toString: String = "read " + x.toString
}
