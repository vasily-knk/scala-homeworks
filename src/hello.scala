import scala.util.parsing.combinator._
import scala.collection.immutable.HashMap

object Arith {
    type Context = Map[String, Double]

    trait Expr {
        def eval(context: Context) : Double
    }

    case class BinaryExpression(left: Expr, op: String, right: Expr) extends Expr {
        def eval(context: Context) : Double = {
            op match {
                case "+" => left.eval(context) + right.eval(context)
                case "-" => left.eval(context) - right.eval(context)
                case "*" => left.eval(context) * right.eval(context)
                case "/" => left.eval(context) / right.eval(context)
            }
        }
    }

    case class LiteralExpression(number: Double) extends Expr {
        def eval(context: Context): Double = number
    }

    case class ParenthesisedExpression(expr: Expr) extends Expr {
        def eval(context: Context): Double = expr.eval(context)
    }

    case class VariableExpression(name: String) extends Expr {
        def eval(context: Context): Double = {
            context.get(name).get
        }
    }

    case class Assignment(name: String, e: Expr) {

    }
}

class Arith extends JavaTokenParsers {
    import Arith._
    def convertToBinary: PartialFunction[ ~[Expr, List[ ~[String, Expr]]], Expr] = {
        case left ~ list =>
            if (list.isEmpty) left
                else list.foldLeft(left) {
                case (res, op ~ right) => BinaryExpression(res, op, right)
            }
    }
    def foo: PartialFunction[ ~[String, Expr], Assignment] = {
        case name ~ e => Assignment(name, e)
    }

    def parseBody(list : List[Assignment]) : Context = {
        var context : Context = new HashMap[String, Double]
        list.map({
            case Assignment(name, e) => context = context + (name -> e.eval(context))
        })
        context
    }

    def expr: Parser[Expr] = term ~ rep("+" ~ term | "-" ~ term) ^^ convertToBinary
    def term: Parser[Expr] = factor ~ rep("*" ~ factor | "/" ~ factor) ^^ convertToBinary
    def factor: Parser[Expr] =
        floatingPointNumber ^^ { case s => LiteralExpression(s.toDouble) } |
        ident               ^^ { case s => VariableExpression(s)} |
        ("(" ~> expr <~ literal(")"))

    def assign: Parser[Assignment] = (ident <~ "=") ~ expr ^^ foo
    def body: Parser[Context] = repsep(assign, ";") ^^ parseBody
}

object Hello extends Arith {
    def main(args: Array[String]) {
        println(parseAll(body, "a = 7 + 8; b = 5 + a"))
    }
}

