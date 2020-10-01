package LettuceVis

import LettuceAST._
import java.awt.{Graphics2D,Color}

object EnvironmentRenderUtils {
    val padding = 5

    def valueToText(v: Value) = v match {
        case NumValue(f) => f.toString
        case BoolValue(b) => b.toString
        case VisualClosure(param, _,_) => s"closure($param, ...)"
        case _ => throw new AssertionError("Cannot handle value type Closure in a visual interpreter -- ask Sriram to debug")
    }


    def renderEnv(env: LettuceEnvironment, g: Graphics2D, x0: Int, y: Int): Unit = {
        val x = x0 + 2* padding
        val (txt, whatsLeft) = env match {
            case EmptyEnvironment => ("NIL", None)
            case ExtendEnv(x, v, rest) =>  (s"$x : ${valueToText(v)}", Some(rest))
            case ExtendEnvRec(_, _, _, _) => throw new AssertionError("Cannot handle a ExtendEnvRec in a visual interpreter -- ask Sriram to debug")
            case VisualExtendEnvRec(funName, argName, _, rest) => (s"$funName -> RecursiveFun($argName, ...)", Some(rest))
        }

        val fm = g.getFontMetrics()
        val textHeight = fm.getHeight()
        val textWidth  = fm.stringWidth(txt)
        g.setColor(Color.BLACK)
        g.drawRect(x-padding, y-padding, textWidth+2*padding, textHeight+2*padding)
        g.drawString(txt, x, y+textHeight)
        whatsLeft match {
            case None => ()
            case Some(rest) => renderEnv(rest, g, x + padding, y + textHeight + 3 * padding)
        }
    }

}
