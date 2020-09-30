package LettuceAST
import scala.util.parsing.combinator.RegexParsers

class LettuceParser extends RegexParsers {
    def floatingPointNumber: Parser[String] = {
        """-?(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?[fFdD]?""".r
    }

    def identifier: Parser[String] = {
        """[a-zA-Z_][a-zA-Z0-9_]*""".r
    }
    def LetKwd: Parser[String] = {
        "let"  | "Let"
    }

    def compOperator: Parser[String] = {
        ">=" | "<=" | ">" | "<" | "==" | "!="
    }

    def funDefinition: Parser[FunDef] = {
         ("function" ~"(") ~> identifier ~ (")" ~> exprLev1)  ^^ {
            case id~e => FunDef(id, e)

        }
    }

    def funCallArgs: Parser[Expr] = {
        "(" ~> exprLev1 <~ ")"
    }

    def exprLev1: Parser[Expr] = {
        val opt1 = ("let" ~> identifier) ~ ("=" ~> exprLev1) ~ ("in" ~> exprLev1)  ^^ {
            case s1 ~ e1 ~ e2 => Let(s1, e1, e2)
        }
        val opt2 = ("letrec" ~> identifier) ~ ("=" ~> funDefinition) ~ ("in" ~> exprLev1 ) ^^ {
            case s1 ~ fd ~ e2 =>
                fd match {
                    case FunDef(id, e1) => LetRec (s1,id, e1, e2)
                    case _ => throw SyntaxError("Unexpected case in letrec definition")
                }
        }
        val opt3 = funDefinition ^^ { s => s }

        val opt4 = ("if" ~> exprLev2)~("then" ~> exprLev1)~("else" ~> exprLev1) ^^ {
            case e ~ e1 ~ e2 => IfThenElse(e, e1, e2)
        }

        val opt5 = exprLev2~ opt(("&&"|"||") ~ exprLev1)^^ {
            case e1 ~ Some("&&" ~ e2) => And(e1, e2)
            case e1 ~ Some("||" ~ e2) => Or(e1, e2)
            case e1 ~ None => e1
        }
        /*val opt5 = exprLev2 ~ compOperator~ exprLev1 ^^{
            case e1~">="~e2 => Geq(e1, e2)
            case e1~"<="~e2 => Geq(e2, e1)
            case e1~"=="~e2 => Eq(e1, e2)
            case e1~"!="~e2 => Neq(e1, e2)
            case e1~">"~e2 => Gt(e1, e2)
            case e1~"<"~e2 => Gt(e2, e1)
        }*/

        opt1 | opt2 | opt3 | opt4 |   opt5
    }

    def exprLev2: Parser[Expr] = {
        exprLev3 ~ opt(compOperator~ exprLev2) ^^{
            case e1~Some(">="~e2) => Geq(e1, e2)
            case e1~Some("<="~e2) => Geq(e2, e1)
            case e1~Some("=="~e2) => Eq(e1, e2)
            case e1~Some("!="~e2) => Neq(e1, e2)
            case e1~Some(">"~e2) => Gt(e1, e2)
            case e1~Some("<"~e2) => Gt(e2, e1)
            case e1~None => e1
        }
    }

    def exprLev3: Parser[Expr] = {
        exprLev4 ~ opt( ("+"| "-") ~ exprLev3 ) ^^ {
            case e1 ~ Some("+" ~ e2) => Plus(e1, e2)
            case e1 ~ Some("-" ~ e2) => Minus(e1, e2)
            case e1 ~ None => e1
        }
    }

    def exprLev4: Parser[Expr] = {

        exprLev5 ~ opt(("*"|"/") ~ exprLev4) ^^ {
            case e1 ~ Some("*" ~ e2) => Mult(e1, e2)
            case e1 ~ Some("/" ~ e2) => Div(e1, e2)
            case e1 ~ None => e1
        }

    }

    def exprLev5: Parser[Expr] = {
        ( floatingPointNumber ^^ { s => ConstNum(s.toFloat)} ) |
          (  "true"^^{ _ => ConstBool(true) } ) |
          ( "false" ^^{ _ => ConstBool(false) } ) |
          (  "(" ~> exprLev1 <~ ")" ) |
          ( ( "sin" | "cos" | "log" | "exp" | "!"  ) ~ ("(" ~> exprLev1 <~ ")") ^^{
              case "sin"~e => Sine(e)
              case "cos"~e => Cosine(e)
              case "log"~e => Log(e)
              case "exp"~e => Exp(e)
              case "!"~e => Not(e)
          } ) |
          ( identifier ~ rep(funCallArgs)  ^^ {
              case s~Nil => Ident(s)
              case s~l => l.foldLeft[Expr] (Ident(s)) { case (e, lj) => FunCall(e, lj) }
          })
    }

    def parseString(s: String): Program = {
        val e= parseAll(exprLev1, s)
        e match {
            case Success(p, _) => TopLevel(p)
            case Failure(msg, _) => throw new IllegalArgumentException("Failure:" + msg)
            case Error(msg, _) => throw new IllegalArgumentException("Error: " + msg)
        }
    }
}
