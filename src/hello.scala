import scala.util.parsing.combinator._
import scala.collection.immutable.HashMap

object Arith {
    trait Value {
        def value: Double
        def literal: String
        case class WrongType(smth:String)  extends Exception
    }

    case class NumValue(value: Double) extends Value {
        def literal: String = throw new WrongType("Double -> String")
    }

    case class LitValue(literal: String) extends Value {
        def value: Double = throw new WrongType("String -> Double")
    }

    type Context = Map[String, Value]

    trait Expr {
        def eval(context: Context) : Value
    }

    case class BinaryExpression(left: Expr, op: String, right: Expr) extends Expr {
        def eval(context: Context) : Value = {
            op match {
                case "+" => NumValue(left.eval(context).value + right.eval(context).value)
                case "-" => NumValue(left.eval(context).value - right.eval(context).value)
                case "*" => NumValue(left.eval(context).value * right.eval(context).value)
                case "/" => NumValue(left.eval(context).value / right.eval(context).value)
            }
        }
    }

    case class NumericExpression(number: Double) extends Expr {
        def eval(context: Context): Value = NumValue(number)
    }

    case class LiteralExpression(string: String) extends Expr {
        def eval(context: Context): Value = LitValue(string)
    }

    case class ParenthesisedExpression(expr: Expr) extends Expr {
        def eval(context: Context): Value = expr.eval(context)
    }

    case class VariableExpression(name: String) extends Expr {
        def eval(context: Context): Value = {
            context.get(name).get
        }
    }

    case class Assignment(name: String, e: Expr)
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
    def foo(name: String, e: Expr) : Assignment = {
        Assignment(name, e)
    }

    def parseBody(list: List[Assignment], expr: Expr) : Value = {
        var context : Context = new HashMap[String, Value]
        list.map({
            case Assignment(name, e) => context = context + (name -> e.eval(context))
        })
        expr.eval(context)
    }

    def expr: Parser[Expr] = term ~ rep("+" ~ term | "-" ~ term) ^^ convertToBinary
    def term: Parser[Expr] = factor ~ rep("*" ~ factor | "/" ~ factor) ^^ convertToBinary

    def factor: Parser[Expr] =
        floatingPointNumber ^^ { case s => NumericExpression(s.toDouble) } |
        ident               ^^ { case s => VariableExpression(s)} |
        ("(" ~> expr <~ ")")

    def assign: Parser[Assignment] = (ident <~ "=") ~ expr ^^ { case name ~ e => Assignment(name, e) }

    def body: Parser[Value] = (repsep(assign, ";") <~ ";") ~ expr ^^ { case list ~ e => parseBody(list, e) }
}

object Hello extends Arith {
    def sum(a:Int, b:Int) = a + b

    def main(args: Array[String]) {
        println(parseAll(body, "a = 7 + 8; a = 5 + a; a * 2"))
    }
}

