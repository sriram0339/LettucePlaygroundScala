package LettuceVis

import LettuceAST._



/* -- In this class, we will attempt to render a programs AST with a focus expression -- */

object ASTRender {

    def renderBinaryOp(expr: Expr, e1: Expr, e2: Expr, str: String): VisualAST = {
        val r1 = renderAST(e1)
        val r2 = renderAST(e2)
        RenderBinOp(expr, r1, r2, str)
    }

    def renderUnaryOp(expr: Expr, expr1: Expr, str: String): VisualAST = {
        val r1 = renderAST(expr1)
        RenderUnOp(expr, r1, str)
    }

    def renderITE(e: Expr, eCond: Expr,eThen: Expr, eElse: Expr): VisualAST = {
        val r1 = renderAST(eCond)
        val r2 = renderAST(eThen)
        val r3 = renderAST(eElse)
        RenderITEExpr(e, r1,r2,r3)
    }

    def renderLet(e: Expr, id: String, e1: Expr, e2: Expr): VisualAST = {
        val r1 = renderAST(e1)
        val r2 = renderAST(e2)
        RenderLetBinding(e, id, r1,r2)
    }

    def renderFunCall(expr: Expr, expr1: Expr, expr2: Expr): VisualAST = {
        val r1 = renderAST(expr1)
        val r2 = renderAST(expr2)
        RenderFunCall(expr, r1, r2)
    }

    def renderFunDef(expr: Expr, str: String, expr1: Expr): VisualAST = {
        val r1 = renderAST(expr1)
        RenderFunDef(expr, str, r1)
    }

    def renderLetRec(expr: Expr, str: String, str1: String, expr1: Expr, expr2: Expr): VisualAST = {
        val r1 = renderAST(expr1)
        val r2 = renderAST(expr2)
        RenderLetRec(expr, str, str1, r1, r2)
    }


    def renderAST(e: Expr): VisualAST  = {
        e match {
            case ConstNum(f) => {
                RenderTextBox(e, f.toString)
            }
            case ConstBool(true) => RenderTextBox(e, "true")
            case ConstBool(false) => RenderTextBox(e, "false")
            case Ident(s) => {
                RenderTextBox(e, s)
            }

            case Plus(e1, e2) => renderBinaryOp(e, e1, e2, "+")
            case Minus(e1, e2) => renderBinaryOp(e, e1, e2, "-")
            case Mult(e1, e2) => renderBinaryOp(e, e1, e2, "*")
            case Div(e1, e2) => renderBinaryOp(e, e1, e2, "/")
            case Geq(e1, e2) => renderBinaryOp(e, e1, e2, ">=")
            case Eq(e1, e2) => renderBinaryOp(e, e1, e2, "==")
            case Neq(e1, e2) => renderBinaryOp(e, e1, e2, "!=")
            case Gt(e1, e2) => renderBinaryOp(e, e1, e2, ">")
            case And(e1, e2) => renderBinaryOp(e, e1, e2, "&&")
            case Or(e1, e2) => renderBinaryOp(e, e1, e2, "||")

            case Log(e1) => renderUnaryOp(e, e1, "log")
            case Exp(e1) => renderUnaryOp(e, e1, "exp")
            case Sine(e1) => renderUnaryOp(e, e1,  "sine")
            case Cosine(e1) => renderUnaryOp(e, e1,  "cosine")
            case Not(e1) => renderUnaryOp(e, e1, "not")

            case IfThenElse(eCond, eThen, eElse) => renderITE(e,eCond, eThen, eElse)

            case Let(ident, e1,e2) => renderLet(e, ident, e1, e2)

            case FunDef(param, eBody) => renderFunDef(e, param, eBody)

            case FunCall(eFun, arg) => renderFunCall(e, eFun, arg)

            case LetRec(f, param, e1, e2) => renderLetRec(e, f, param, e1, e2)

        }
    }


}
