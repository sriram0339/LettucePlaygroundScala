package LettuceVis

import java.awt.{BasicStroke, Color, Graphics2D}

import LettuceAST._

/* -- Modify closures to something called a visual closure --*/
case class VisualClosure(param: String, r: VisualAST, env: LettuceEnvironment) extends Value

/* -- Make a visual extension to the extendrec environment to something called a visual closure --*/
case class VisualExtendEnvRec(f: String, param: String, r: VisualAST,  eHat: LettuceEnvironment) extends LettuceEnvironment {
    override def lookup(s: String): Value = {
        if (s == f){
            VisualClosure(param, r, this)
        } else {
            eHat.lookup(s)
        }
    }
}


sealed trait Kontinuation
case class KDone(v: Value, rDone: VisualAST, prevEnv: LettuceEnvironment) extends Kontinuation
case class KEvalExpr(r: VisualAST, env: LettuceEnvironment) extends Kontinuation
case class KFun1 ( fun: Value => List[Kontinuation]) extends Kontinuation


sealed trait TrampolineVisualInterpreterState
case class TrampolineInterpreterRunning (r: VisualAST, env: LettuceEnvironment, kList: List[Kontinuation]) extends TrampolineVisualInterpreterState
case class TrampolineInterpreterDone(v: Value) extends TrampolineVisualInterpreterState
case class TrampolineApplyKont(lst: List[Kontinuation]) extends TrampolineVisualInterpreterState


object TrampolinedVisualInterpreter {

    def stepExprWithVisuals(r: VisualAST, env: LettuceEnvironment): List[Kontinuation] = {
        val expr = r.e
        /* -- Highlight the appropriate text area --*/

        def doBinOp(fun: Value => Value => Value ): List[Kontinuation] = {
            List( KEvalExpr(r.getFirst, env), // First eval argument # 1
                KFun1( v1 => List(KEvalExpr(r.getSecond, env), // Next eval argument # 2
                    KFun1( v2 => List( KDone(fun (v1) (v2 ) , r, env) ) ) // Finally, add them
                ))
            )
        }

        def doUnOp(fun: Value => Value ): List[Kontinuation] = {
            List( KEvalExpr(r.getFirst, env),
                KFun1( v1 => List(KDone( fun(v1), r, env)))
            )
        }

        expr match {
            case ConstNum(f) => List(KDone(NumValue(f), r, env))
            case Ident(s) => {
                List(KDone(env.lookup(s), r, env ))
            }
            case Plus(_, _) => {
                doBinOp(LettuceValue.plus)
            }

            case Minus(_, _) => {
                doBinOp(LettuceValue.minus)
            }

            case Mult(_, _) => {
                doBinOp(LettuceValue.mult)
            }
            case Div(_, _) => {
                doBinOp(LettuceValue.div)
            }
            case Log(_) => doUnOp(LettuceValue.log)
            case Exp(_) => doUnOp(LettuceValue.exp)
            case Sine(_) => doUnOp(LettuceValue.sine)
            case Cosine(_) => doUnOp(LettuceValue.cosine)

            case Geq(_, _) => doBinOp(LettuceValue.geq)
            case Gt(_, _) => doBinOp(LettuceValue.gt)
            case Eq(_, _) => doBinOp(LettuceValue.eq)
            case Neq(_, _) => doBinOp(LettuceValue.neq)

            case And(_, _) => { // Implement the short circuiting
                List(KEvalExpr(r.getFirst, env), // First evaluate the first subexpression
                    KFun1({ // case match on the outcome of the first evaluation
                        case BoolValue(false) => List(KDone(BoolValue(false), r, env )) // it is false -- short circuit
                        case BoolValue(true) => List(KEvalExpr(r.getSecond, env)) // it is true, continue on to second
                        case _ => throw new TypeConversionError("Cannot convert non-boolean type to boolean for AND")
                    }))
            }

            case Or(_, _) => {
                List(KEvalExpr(r.getFirst, env), // First evaluate the first subexpression
                    KFun1({ // case match on the outcome of the first evaluation
                        case BoolValue(true) => List(KDone(BoolValue(true), r, env )) // it is true -- short circuit
                        case BoolValue(false) => List(KEvalExpr(r.getSecond, env)) // it is false, continue on to second
                        case _ => throw new TypeConversionError("Cannot convert non-boolean type to boolean for OR")
                    }))
            }

            case Not(_) => doUnOp(LettuceValue.not)

            case IfThenElse(_, _, _) => {
                List(
                    KEvalExpr(r.getFirst, env),
                    KFun1({
                        case BoolValue(true) => List(KEvalExpr(r.getSecond, env))
                        case BoolValue(false) => List(KEvalExpr(r.getThird, env))
                        case _ => throw new TypeConversionError("Cannot convert non-boolean type to boolean for if then else")
                    })
                )
            }

            case Let(param, _, _) => {
                List(
                    KEvalExpr(r.getFirst, env),
                    KFun1(v => List(
                     KEvalExpr(r.getSecond, ExtendEnv(param, v, env))
                    ))
                )
            }

            case FunDef(name, _) => {
                List(KDone(VisualClosure(name, r.getFirst, env), r, env))
            }

            case FunCall(_, _) => {
                List(
                    KEvalExpr(r.getFirst, env),
                    KFun1({
                        case VisualClosure(param, rBody, closureEnv) => {
                            List(
                                KEvalExpr(r.getSecond, env),
                                KFun1( vArg => List(
                                    KEvalExpr(rBody, ExtendEnv(param, vArg, closureEnv))
                                ) )
                            )
                        }
                        case _ => throw new TypeConversionError("Cannot convert non-closure value into a function to be called")
                    })
                )
            }

            case LetRec(f, argName, _, _) => {
                val newEnv = VisualExtendEnvRec(f, argName, r.getFirst, env)
                List(
                    KEvalExpr(r.getSecond, newEnv)
                )
            }
        }

    }


    def initTrampolineInterpreter(r: VisualAST): TrampolineVisualInterpreterState = {
        TrampolineInterpreterRunning(r, EmptyEnvironment, Nil)
    }

    def applyKontinuation (lst: List[Kontinuation]): TrampolineVisualInterpreterState = lst match {
        case KDone(v, _,_)::KFun1 (fun1)::rest => {
            val newList = fun1(v)
            //applyKontinuation (newList ++ rest)
            TrampolineApplyKont(newList ++ rest)
        }

        case KEvalExpr(r, env) :: rest => {
            TrampolineInterpreterRunning(r, env, rest)
        }

        case KDone(v, _,_)::Nil => {
            TrampolineInterpreterDone(v)
        }

        case _ => throw new AssertionError("Something that I did not handle/expect is happening... Ask Sriram to Debug")
    }

    def nextState(state: TrampolineVisualInterpreterState): TrampolineVisualInterpreterState = state match {
        case TrampolineInterpreterRunning(r, env, klst) => {
            val newStuff = stepExprWithVisuals(r, env)
            TrampolineApplyKont(newStuff ++ klst)
        }
        case TrampolineApplyKont(lst) => {
            val newState = applyKontinuation(lst)
            newState match {
                case TrampolineInterpreterRunning(_, _, _) => nextState(newState)
                case _ => newState
            }
        }

        case TrampolineInterpreterDone(v) => TrampolineInterpreterDone(v)
    }

    def isDone(state: TrampolineVisualInterpreterState) : Boolean = state match {
        case TrampolineInterpreterDone(_) => true
        case _ => false
    }


    def highlightState(g: Graphics2D, state: TrampolineVisualInterpreterState) = state match {

        case TrampolineInterpreterRunning(r, env, klst) => {
            val stroke = g.getStroke()
            g.setColor(Color.RED)
            g.setStroke(new BasicStroke(3))
            r.highlight(g)
           // r.render(g, r.getCoords())
            g.drawString( "ENVIRONMENT: ", 1000, 40)
            EnvironmentRenderUtils.renderEnv(env, g, 1000, 50)
            g.setStroke(stroke)
        }

        case TrampolineApplyKont(lst) => {
            if (lst.length >= 1) {
                lst.head match {
                    case KDone(v, r, env ) => {
                        val vStr = s"Value: ${EnvironmentRenderUtils.valueToText(v)}"
                        val stroke = g.getStroke()
                        g.setColor(Color.RED)
                        g.setStroke(new BasicStroke(3))
                        r.highlight(g)
                        r.setTextTop(g, vStr)
                        g.drawString("ENVIRONMENT: ", 1000, 40)
                        EnvironmentRenderUtils.renderEnv(env, g, 1000, 50)
                        g.setStroke(stroke)
                    }
                    case KEvalExpr(r, env) => {
                        val stroke = g.getStroke()
                        g.setColor(Color.RED)
                        g.setStroke(new BasicStroke(3))
                        r.highlight(g)
                        g.drawString("ENVIRONMENT: ", 1000, 40)
                        EnvironmentRenderUtils.renderEnv(env, g, 1000, 50)
                        g.setStroke(stroke)
                    }
                }
            }
        }

        case TrampolineInterpreterDone(v) => {
            g.setColor(Color.RED)
            g.drawString(s"Finished Running with result: ${EnvironmentRenderUtils.valueToText(v)}", 1000, 40)
        }
    }

}

