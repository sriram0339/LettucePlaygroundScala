package LettuceAST

/*-
    This file has the AST definitions for Lettuce.
    TODO: add line number information in our AST since we also want
    TODO: to be able to parse and provide better error messages.
 */
sealed trait Program
sealed trait Expr

/* Program => TopLevel(Expr) */
case class TopLevel(e: Expr) extends Program

/* Expr => Const */
case class ConstNum(f: Double) extends Expr
case class ConstBool(b: Boolean) extends Expr

/* Expr => Ident */
case class Ident(s: String) extends Expr

/* Arithmetic Operators */
case class Plus(e1: Expr, e2: Expr) extends Expr
case class Minus(e1: Expr, e2: Expr ) extends Expr
case class Mult(e1: Expr, e2: Expr) extends Expr
case class Div(e1: Expr, e2: Expr) extends Expr
case class Log(e: Expr) extends Expr
case class Exp(e: Expr) extends Expr
case class Sine(e: Expr) extends Expr
case class Cosine(e: Expr) extends Expr

/* Comparison Operators */
case class Geq(e1: Expr, e2: Expr) extends Expr
case class Eq(e1: Expr, e2: Expr) extends Expr
case class Gt(e1: Expr, e2: Expr) extends Expr
case class Neq(e1: Expr, e2: Expr) extends Expr
/* Booleans */
case class And(e1: Expr, e2: Expr) extends Expr
case class Or(e1: Expr, e2: Expr) extends Expr
case class Not(e1: Expr) extends Expr

/*If then Else */
case class IfThenElse(e: Expr, e1: Expr, e2: Expr) extends Expr


/* Let Binding */
case class Let(x: String, e1: Expr, e2: Expr) extends Expr

/* Function definition */
/* Note that we will allow our functions to have multiple parameters */
case class FunDef(name: String, e: Expr) extends Expr
/* Function call */
case class FunCall(e: Expr, arg: Expr) extends Expr

/* Recursive Definition */
case class LetRec(f: String, argName: String, e1: Expr, e2: Expr) extends Expr

