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

    type Vars = Map[String, Value]
    type Funcs = Map[String, Func]

    case class Context(vars: Vars, funcs: Funcs)

    trait Expr {
        def eval(context: Context) : Value
    }

    case class BinaryExpression(left: Expr, op: String, right: Expr) extends Expr {
        def eval(context: Context) : Value = {
            val values = (left.eval(context), right.eval(context))

            values match {
                case (LitValue(s1), LitValue(s2)) =>
                    op match {
                        case "+" => LitValue(s1 + s2)
                    }
                case (NumValue(leftVal), NumValue(rightVal)) =>
                    op match {
                        case "+" => NumValue(leftVal + rightVal)
                        case "-" => NumValue(leftVal - rightVal)
                        case "*" => NumValue(leftVal * rightVal)
                        case "/" => NumValue(leftVal / rightVal)
                    }
            }
        }
    }

    case class NumericExpression(number: Double) extends Expr {
        def eval(context: Context): Value = NumValue(number)
    }

    case class LiteralExpression(string: String) extends Expr {
        def eval(context: Context): Value = LitValue(string.substring(1, string.length - 1))
    }

    case class ParenthesisedExpression(expr: Expr) extends Expr {
        def eval(context: Context): Value = expr.eval(context)
    }

    case class VariableExpression(name: String) extends Expr {
        case class VarNotFound(smth:String) extends Exception

        def eval(context: Context): Value = {
            if (!context.vars.contains(name))
                throw VarNotFound("Variable not found: " + name)
            else
                context.vars.get(name).get
        }
    }

    case class Body(assignments: List[Assignment], expr: Expr) extends Expr {
        def eval(context: Context): Value = {
            var newContext = context
            assignments.map({
                case Assignment(name, e) => newContext = Context(newContext.vars + (name -> e.eval(newContext)), newContext.funcs)
            })

            expr.eval(newContext)
        }
    }

    case class Func(name: String, args: Args, body: Body) {
        case class WrongArgsNum(smth:String) extends Exception

        def call(argValues: ArgValues, funcs: Funcs) : Value = {
            if (argValues.length != args.length)
                throw WrongArgsNum("Wrong number of args")

            var vars = new HashMap[String, Value]
            (args zip argValues).map({
                case (argName, argValue) => vars += (argName -> argValue)
            })

            body.eval(Context(vars, funcs))
        }
    }

    case class Assignment(name: String, e: Expr)

    case class CallExpr(name: String, args: CallArgs) extends Expr {
        def eval(context: Context): Value = {
            val argValues = args.map((e: Expr) => e.eval(context))
            context.funcs.get(name).get.call(argValues, context.funcs)
        }
    }

    object Prog {
        def list2map(list: List[Func]): Funcs = {
            var funcs = new HashMap[String, Func]()
            list.map({
                case f => funcs += (f.name -> f)
            })
            funcs
        }
    }

    class Prog(private val funcs: Funcs) {
        def this(list: List[Func]) = {
            this(Prog.list2map(list))
        }

        def run: Value = {
            funcs.get("main").get.call(List(), funcs)
        }
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

    def expr: Parser[Expr] = term ~ rep("+" ~ term | "-" ~ term) ^^ convertToBinary
    def term: Parser[Expr] = factor ~ rep("*" ~ factor | "/" ~ factor) ^^ convertToBinary

    def callArgs: Parser[CallArgs] = "(" ~> repsep(expr, ",") <~ ")"

    def call: Parser[Expr] = ident ~ callArgs ^^ {case name ~ args => CallExpr(name, args)}

    def factor: Parser[Expr] =
        floatingPointNumber ^^ { case s => NumericExpression(s.toDouble) } |
        stringLiteral       ^^ LiteralExpression  |
        call |
        ident               ^^ VariableExpression |
        ("(" ~> expr <~ ")")

    def assign: Parser[Assignment] = (ident <~ "=") ~ expr ^^ { case name ~ e => Assignment(name, e) }

    def body: Parser[Body] = ("{" ~> rep(assign <~ ";")) ~ expr <~ "}" ^^ { case list ~ e => Body(list, e) }

    def args: Parser[Args] = "(" ~> repsep(ident, ",") <~ ")"

    def func: Parser[Func] = ("def" ~> ident) ~ args ~ body ^^ {case name ~ args ~ body => Func(name, args, body) }

    def prog: Parser[Prog] = rep(func) ^^ { case list => new Prog(list) }
}

object Hello extends Arith {
    def sum(a:Int, b:Int) = a + b

    def main(args: Array[String]) {

        val text = """
                     | def sum(a, b) { c = a + b; c }
                     | def main() { string = "Hello"; string = sum(string, " world"); string }
                   """.stripMargin

        println(parseAll(prog, text).get.run)

    }
}

