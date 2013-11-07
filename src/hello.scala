import scala.util.parsing.combinator._
import scala.collection.immutable._

object Arith {
    trait Value {
        def value: Double
        def literal: String
        case class WrongType(smth:String) extends Exception
    }

    type Args = List[String]
    type CallArgs = List[Expr]
    type ArgValues = List[Value]

    case class NumValue(value: Double) extends Value {
        def literal: String = throw new WrongType("Double -> String")
    }

    case class LitValue(literal: String) extends Value {
        def value: Double = throw new WrongType("String -> Double")
    }

    case class Context(vars: Map[String, Value])

    trait Expr {
        def eval(context: Context) : Value
    }

    case class BinaryExpression(left: Expr, op: String, right: Expr) extends Expr {
        def eval(context: Context) : Value = {
            val leftVal  = left.eval(context).value
            val rightVal = left.eval(context).value
            op match {
                case "+" => NumValue(leftVal + rightVal)
                case "-" => NumValue(leftVal - rightVal)
                case "*" => NumValue(leftVal * rightVal)
                case "/" => NumValue(leftVal / rightVal)
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
            context.vars.get(name).get
        }
    }

    case class Body(assignments: List[Assignment], expr: Expr)

    case class Func(name: String, args: Args, body: Body) {

    }

    case class Assignment(name: String, e: Expr)

    case class CallExpr(name: String, args: CallArgs) extends Expr {
        def eval(context: Context): Value = {
            //val arg_values = args.map((e: Expr) => e.eval(context))
            NumValue(0)
        }
    }

    class Prog(funcs: List[Func]) {


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

    def parseBody(list: List[Assignment], expr: Expr) : Value = {
        var context = Context(new HashMap[String, Value])
        list.map({
            case Assignment(name, e) => context = Context(context.vars + (name -> e.eval(context)))
        })
        expr.eval(context)
    }

    def expr: Parser[Expr] = term ~ rep("+" ~ term | "-" ~ term) ^^ convertToBinary
    def term: Parser[Expr] = factor ~ rep("*" ~ factor | "/" ~ factor) ^^ convertToBinary

    def callArgs: Parser[CallArgs] = "(" ~> repsep(expr, ",") <~ ")"

    def call: Parser[Expr] = ident ~ callArgs ^^ {case name ~ args => CallExpr(name, args)}

    def factor: Parser[Expr] =
        floatingPointNumber ^^ { case s => NumericExpression(s.toDouble) } |
        stringLiteral       ^^ LiteralExpression  |
        ident               ^^ VariableExpression |
        ("(" ~> expr <~ ")")

    def assign: Parser[Assignment] = (ident <~ "=") ~ expr ^^ { case name ~ e => Assignment(name, e) }

    def body: Parser[Body] = ("{" ~> rep(assign <~ ";")) ~ expr <~ "}" ^^ { case list ~ e => Body(list, e) }

    def args: Parser[Args] = "(" ~> repsep(ident, ",") <~ ")"

    def func: Parser[Func] = ("def" ~> ident) ~ args ~ body ^^ {case name ~ args ~ body => Func(name, args, body) }

    def prog: Parser[Prog] = rep(func) ^^ Prog
}

object Hello extends Arith {
    def sum(a:Int, b:Int) = a + b

    def main(args: Array[String]) {

        val text = """
                     | def sum(a, b) { c = a + b; c }
                     | def hello() { "Hello!" }
                   """.stripMargin

        println(parseAll(prog, text))
    }
}

