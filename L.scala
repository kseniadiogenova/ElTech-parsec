import scala.io.Source

object L {
  def main(args: Array[String]) {
    val inputFile = Source.fromFile(args(0))
    val inputSource = inputFile.mkString

    //val parser = new LParser
    val parser = new Lparse
    parser.parseAll(parser.prog, inputSource) match {
      case parser.Success(r, n) => {
        val interpreter = new Interprete(r)

        try {
          interpreter.run
        } catch {
          case e: RuntimeException => println(e.getMessage)
        }
      }
      case parser.Error(msg, n) => println("Error: " + msg)
      case parser.Failure(msg, n) => println("Error: " + msg)
      case _ =>
    }
  }
}