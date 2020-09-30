package LettuceVis

import LettuceAST._
import java.awt.{FontMetrics, Graphics2D}



sealed trait VisualAST {
    // Add some definitions
    val e: Expr
    def render(g: Graphics2D, coords: (Float, Float)): (Float, Float)
    val padding = 5.0f
    var xLeft: Float = 0.0f
    var yTop: Float = 0.0f
    var width: Float = 0.0f
    var height: Float = 0.0f

    def highlight(g:Graphics2D): Unit = {
        g.drawRect(xLeft.toInt, yTop.toInt, width.toInt, height.toInt)
    }

    def setTextTop(g: Graphics2D, txt: String) : Unit = {
        val x = xLeft.toInt + width.toInt -  5 * txt.size
        val y  =yTop - padding
        g.drawString(txt, x, y)
    }

    def getFirst: VisualAST = throw new IllegalArgumentException("Cannot be called")
    def getSecond: VisualAST = throw new IllegalArgumentException("Cannot be called")
    def getThird: VisualAST = throw new IllegalArgumentException("Cannot be called")

}

case class RenderTextBox( e: Expr, txt: String) extends VisualAST {

    def render(g: Graphics2D, coords: (Float, Float)): (Float, Float) = {
        val (x, y) = coords
        val fm: FontMetrics = g.getFontMetrics()
        val hgt: Float = fm.getHeight().toFloat
        val adv:Float  = fm.stringWidth(txt).toFloat
        g.drawString(txt, x, y + hgt)
        val (finalWidth, finalHeight) = ( adv + padding ,  hgt + padding )
        this.xLeft = x
        this.yTop = y
        this.width = finalWidth
        this.height = finalHeight
        (finalWidth, finalHeight)
    }


}

case class RenderBinOp( e: Expr, r1: VisualAST, r2: VisualAST, opStr: String ) extends VisualAST {

    def render(g: Graphics2D, coords: (Float, Float)): (Float, Float) = {
        val (x1, y1) = coords
        val (wd, ht) = r1.render(g, (x1 , y1 ))
        val fm: FontMetrics = g.getFontMetrics()
        val strhgt: Float = fm.getHeight().toFloat
        val stradv:Float  = fm.stringWidth(opStr).toFloat
        g.drawString ( opStr, x1 + wd + padding, y1  + strhgt)
        val (x2, y2) = (x1 + wd + padding + stradv + padding, y1)
        val (wd2, ht2) = r2.render(g, (x2, y2))
        val (finalWidth, finalHeight) =(wd + wd2 + 3*padding + stradv, List(ht, ht2, strhgt).max + padding )

        this.xLeft = x1
        this.yTop = y1
        this.width= finalWidth
        this.height = finalHeight
        (finalWidth, finalHeight)
    }

    override def getFirst: VisualAST = r1
    override def getSecond: VisualAST = r2
}

case class RenderUnOp(e: Expr, r: VisualAST, opStr: String) extends VisualAST {

    def render(g: Graphics2D, coords: (Float, Float)): (Float, Float) = {
        val (x1, y1 ) = coords
        val fm: FontMetrics = g.getFontMetrics()
        val strhgt: Float = fm.getHeight().toFloat
        val stradv:Float  = fm.stringWidth(opStr).toFloat
        g.drawString ( opStr, x1 +  padding, y1  + strhgt)
        val (x2, y2) = (x1 + stradv + 3 * padding, y1)
        val (wd, ht) = r.render(g, (x2, y2))
        val (finalWidth, finalHeight) = (wd  + 3 * padding + stradv, List(strhgt, ht).max + padding )
        this.xLeft = x1
        this.yTop = y1
        this.width= finalWidth
        this.height = finalHeight
        (finalWidth, finalHeight)
    }

    override def getFirst: VisualAST = r

}

case class RenderITEExpr(e: Expr, rCond: VisualAST, rThen: VisualAST, rElse: VisualAST) extends VisualAST {
    def render(g: Graphics2D, coords: (Float, Float)): (Float, Float) = {
        val (x1, y1 ) = coords
        val fm = g.getFontMetrics()
        val textHeight : Float = fm.getHeight(). toFloat
        val ifWidth : Float = fm.stringWidth("if ").toFloat
        val thenWidth: Float = fm.stringWidth("then").toFloat
        val elseWidth: Float = fm.stringWidth("else").toFloat
        val maxTextWidth: Float = List(ifWidth, thenWidth, elseWidth).max + padding

        g.drawString("if ", x1, y1 + textHeight)
        val (wdCond, htCond) = rCond.render(g, (x1 + maxTextWidth + padding, y1))
        val y2 = y1 + htCond + 3 * padding
        g.drawString("then", x1, y2 + textHeight)
        val (wdThen, htThen) = rThen.render(g,(x1+ maxTextWidth + padding, y2))
        val y3 = y2 + htThen + 3 * padding
        g.drawString("else", x1, y3 + textHeight)
        val (wdElse, htElse) = rElse.render(g, (x1 + maxTextWidth + padding, y3))

        val (finalWidth, finalHeight) = (List(wdCond, wdThen, wdElse).max + maxTextWidth + 2*padding, htCond + htThen + htElse + 10 * padding )
        this.xLeft = x1
        this.yTop = y1
        this.width= finalWidth
        this.height = finalHeight
        this.highlight(g)
        (finalWidth, finalHeight)
    }

    override def getFirst: VisualAST = rCond
    override def getSecond: VisualAST = rThen
    override def getThird: VisualAST = rElse
}

case class RenderLetBinding(e: Expr, id: String, r1: VisualAST ,r2: VisualAST ) extends VisualAST {
    def render(g: Graphics2D, coords: (Float, Float)): (Float, Float) = {
        val (x1, y1 ) = coords
        val fm = g.getFontMetrics()
        val textHeight : Float = fm.getHeight(). toFloat
        val letWidth : Float = fm.stringWidth(s"LET $id = ").toFloat
        val inWidth: Float = fm.stringWidth("IN").toFloat
        val maxTextWidth: Float = List(letWidth, inWidth).max + padding
        g.drawString(s"let $id = ", x1, y1 + textHeight)
        val (wdLet, htLet) = r1.render(g, (x1 + maxTextWidth + padding, y1 + 3* padding ))
        val y2 = y1 + htLet + 4* padding
        g.drawString("in ", x1, y2 + textHeight)
        val (wdIn, htIn) = r2.render(g, (x1 + maxTextWidth + padding, y2))
        val (finalWidth, finalHeight) = (List(wdLet, wdIn).max + maxTextWidth + 2  * padding, htLet + htIn + 6 * padding)
        this.xLeft = x1
        this.yTop = y1
        this.width= finalWidth
        this.height = finalHeight
        this.highlight(g)
        (finalWidth, finalHeight)
    }

    override def getFirst: VisualAST = r1
    override def getSecond: VisualAST = r2
}

case class RenderFunCall(e: Expr, rFun: VisualAST, rArg: VisualAST) extends VisualAST {
    def render(g: Graphics2D, coords: (Float, Float)): (Float, Float) = {
        val (x1, y1) = coords
        val (wdFun, htFun ) = rFun.render (g, coords)
        val fm = g.getFontMetrics()
        val textHeight : Float = fm.getHeight(). toFloat
        val textWidth : Float = fm.stringWidth("()").toFloat
        g.drawString("(", x1 + wdFun + padding , y1 + textHeight)
        val (wdArg, htArg) = rArg.render(g, (x1 + wdFun +textWidth, y1))
        g.drawString(")", x1 + wdFun + textWidth+wdArg, y1 + textHeight)
        val (finalWidth, finalHeight) = (wdFun + wdArg + 2 * textWidth + padding, List(textHeight, htFun, htArg).max + padding)
        this.xLeft = x1
        this.yTop = y1
        this.width= finalWidth
        this.height = finalHeight
        (finalWidth, finalHeight)
    }

    override def getFirst: VisualAST = rFun
    override def getSecond: VisualAST = rArg
}

case class RenderFunDef(e: Expr, param: String, body: VisualAST) extends VisualAST {
    def render(g: Graphics2D, coords: (Float, Float)): (Float, Float) = {
        val (x1, y1) = coords
        val paramstr = s"function ($param) "
        val fm = g.getFontMetrics()
        val textHeight : Float = fm.getHeight(). toFloat
        val textWidth : Float = fm.stringWidth(paramstr)
        g.drawString(paramstr, x1, y1+textHeight)
        val (wdBody, htBody) = body.render(g, (x1 + textWidth+padding, y1 + 5* padding))
        val (finalWidth, finalHeight) = (wdBody + textWidth + padding, List(htBody, textHeight).max + 6*padding)
        this.xLeft = x1
        this.yTop = y1
        this.width= finalWidth
        this.height = finalHeight
        this.highlight(g)
        (finalWidth, finalHeight)
    }

    override def getFirst: VisualAST = body
}

case class RenderLetRec(e: Expr, funName: String, param:String , r1: VisualAST, r2: VisualAST) extends VisualAST {

    def render(g: Graphics2D, coords:(Float, Float)): (Float, Float) = {
        val (x1, y1) = coords
        val paramstr = s"let rec $funName = fun ($param)"
        val fm = g.getFontMetrics()
        val textHeight : Float = fm.getHeight(). toFloat
        val textWidth : Float = fm.stringWidth(paramstr)
        g.drawString(paramstr, x1, y1 + textHeight)
        val (wd1, ht1) = r1.render(g, (x1 + textWidth + padding, y1 + 5 * padding ))
        val y2 = y1 + ht1 + 3* padding
        g.drawString("in ", x1, y2 + textHeight)
        val (wd2, ht2) = r2.render(g, (x1 + textWidth+padding, y2 + padding ))

        val (finalWidth, finalHeight) = (List(wd1,wd2).max + textWidth+padding, ht1 + ht2  + 6 * padding)
        //g.drawRect(x1.toInt, y1.toInt, finalWd.toInt + 1, finalHt.toInt + 1)
        this.xLeft = x1
        this.yTop = y1
        this.width= finalWidth
        this.height = finalHeight
        this.highlight(g)
        (finalWidth, finalHeight)
    }

    override def getFirst: VisualAST = r1
    override def getSecond: VisualAST = r2

}



