import scala.util.parsing.combinator.syntactical.StandardTokenParsers;
import ast._;

class Lparse extends StandardTokenParsers{
  lexical.reserved +=("skip", "read", "write", "while", "do", "od", "if", "then", "else", "fi")
  lexical.delimiters +=("+" , "-" , "**" , "*" , "/" , "%" , "==" , "!=" , ">" , ">=" , "<" , "<=","&&" , "||", "(", ")", ":=", ";")
  protected val whiteSpace = """\s | //.*""".r

  //этот метод будет парсить переменные
  def variable : Parser[Variable] = ident ^^ {Variable(_)}
  //этот будет парсить числа
  def number : Parser[Number] = numericLit ^^ {
    x => Number(x.toInt)
  }
  //все возможные операции - отдельный парсер
  def operation : Parser[String] = "+" | "-" | "**" | "*" | "/" | "%" | "==" | "!=" | ">" | ">=" | "<" | "<="|"&&" | "||"
  //Выражение - оператор
  def operator : Parser[Operator] = partexp ~ operation ~ expr ^^{
    case e1 ~ op ~ e2 => new Operator(e1,e2, op)
  }
  //Выражение - (exp)
  def brakets : Parser[Brakets] = "(" ~ expr ~ ")" ^^ {
    case _ ~ exp ~ _ => new Brakets(exp)
  }
  //Частичное выражение - все, кроме operator, для избежания левой рекурсии
  def partexp : Parser[Expr] = variable | number | brakets
  //Выражение - по правилу из задания
  def expr : Parser[Expr] = operator |variable | number | brakets

  //----- С выражениями на этом все -------
  //Парсинг программы

  //операция присвоения X := expr
  def set : Parser[Set] = variable ~ ":=" ~ expr ^^  {
    case id ~ exp => new Set(id._1, exp)
  }
  //операция чтения
  def read : Parser[Read] = "read" ~ variable ^^{
    case  v => new Read(v._2)
  }
  //похожим образом операция записи
  def write : Parser[Write] = "write" ~ expr ^^ {
    case exp => new Write(exp._2)
  }
  //Пропуск
  def skipstr : Parser[String] = "skip"
  def skip : Parser[Skip] = skipstr ^^{
    case str => new Skip(str)
  }
  //Точка с запятой - s;s
  def colon : Parser[Colon] = partprog ~ ";" ~ prog ^^{
    case pl ~ set ~ pr => new Colon(pl, pr)
  }
  //Цикл
  def loop : Parser[Loop] = "while" ~ expr ~"do" ~ prog ~ "od" ^^{
    case w~exp~d~pr~o => new Loop(exp, pr)
  }
  //Условие if
  def condition : Parser[If] = "if" ~ expr ~ "then" ~ prog ~ "else" ~prog ~"fi" ^^{
    case i~exp~t~prl~e~prr ~f => new If(exp, prl, prr)
  }

  def partprog : Parser[Prog] = set | read | write | skip | loop | condition
  def prog : Parser[Prog] = colon | set | read | write | skip | loop | condition

  def parseAll[T](p: Parser[T], in: String): ParseResult[T] = {
    phrase(p)(new lexical.Scanner(in))
  }

}
