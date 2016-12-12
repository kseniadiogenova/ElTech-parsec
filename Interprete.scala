import ast._

class Interprete(prog : Prog) {
  def run() {
   // println("S")
    walk(prog, 0)
  }

  def printtabs(tabs : Int): Unit ={
    for( i <- 0 to tabs){
      print("\t")
    }
  }

  def walk(prog: Prog, tabs : Int): Unit ={
    prog match {
      case Skip(sk) => {
        printtabs(tabs)
        println("S")
        printtabs(tabs + 1)
        println("skip")
      }
      case Read(x) => {
        printtabs(tabs)
        println("S")
        printtabs(tabs + 1)
        println("read")
        walkvar(x,tabs+1)
      }
      case Write(exp) => {
        printtabs(tabs)
        println("S")
        printtabs(tabs + 1)
        println("write")
        walkvar(exp,tabs+2)
      }
      case Set(x, exp) => {
        printtabs(tabs)
        println("S")
        walkvar(x,tabs+1)
        printtabs(tabs+1)
        println(":=")
        printtabs(tabs+2)
        println("expr")
        walkvar(exp, tabs+3)
      }
      case Loop(condition, prog) => {
        printtabs(tabs)
        println("S")
        printtabs(tabs+1)
        println("while")
        walkvar(condition, tabs+2)
        printtabs(tabs+1)
        println("do")
        walk(prog, tabs+2)
        printtabs(tabs+1)
        println("od")
      }
      case If(condition, progl, progr)=>{
        printtabs(tabs)
        println("S")
        printtabs(tabs+1)
        println("if")
        walkvar(condition, tabs+2)
        printtabs(tabs+1)
        println("then")
        walk(progl, tabs+2)
        printtabs(tabs+1)
        println("else")
        walk(progr, tabs+2)
        printtabs(tabs+1)
        println("fi")
      }
      case Colon(pl,pr) => {
        printtabs(tabs)
        println("S")
        walk(pl, tabs+1)
        printtabs(tabs+1)
        println(";")
        walk(pr, tabs+1)
      }
    }
  }

  def walkvar(expr: Expr, tabs : Int): Unit ={

    expr match{
      case Variable(name) =>{
        printtabs(tabs)
        println("var")
        printtabs(tabs+1)
        println(name)
      }
      case Number(value) =>{
        printtabs(tabs)
        println("num")
        printtabs(tabs+1)
        println(value)
      }
      case Operator(el, er, op) => {
        printtabs(tabs)
        println("expr")
        walkvar(el, tabs+1)
        printtabs(tabs)
        println(op)
        printtabs(tabs)
        println("expr")
        walkvar(er, tabs+1)
      }
      case Brakets(expr)=>{
        printtabs(tabs)
        println("(")
        printtabs(tabs)
        println("expr")
        walkvar(expr, tabs+1)
        printtabs(tabs)
        println(")")
      }
    }

  }
}
