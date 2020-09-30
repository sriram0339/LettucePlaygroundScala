package LettuceVis



import java.awt.{Color, Font}

import LettuceAST._


import scala.swing._
import scala.swing.BorderPanel.Position._
import scala.swing.{BorderPanel, Button, MainFrame, SimpleSwingApplication}
import scala.swing.event.{ButtonClicked, MouseClicked}

/*--
    The canvas and the basic Swing application GUI for drawing the results of the program.
    Author: Sriram Sankaranarayanan <srirams@colorado>
    Do not edit.
 */

class DrawingCanvas extends Panel{
    var canvas: Option[VisualAST] = None
    var interp: TrampolineVisualInterpreterState = TrampolineInterpreterDone(NumValue(-111.111))
    var stateStack : List[TrampolineVisualInterpreterState] = Nil

    override def paintComponent(g: Graphics2D) = {
        g.setFont(new Font("Helvetica", Font.PLAIN, 20))
        g.setBackground(Color.WHITE)
        canvas match {
            case Some(c) => {
                c.render(g, (25.0f, 25.0f))
                TrampolinedVisualInterpreter.highlightState(g, interp)
            }
            case _ => {
                g.drawString("Press START to start the interpreter", 50, 50)
            }
        }

    }

    def setAST(e: Expr) = {
        canvas = Some(ASTRender.renderAST(e))
        interp = TrampolinedVisualInterpreter.initTrampolineInterpreter(canvas.get)
        stateStack = Nil
    }

    def step(): Unit = {
        if (!TrampolinedVisualInterpreter.isDone(interp)) {
            stateStack = interp :: stateStack
            interp = TrampolinedVisualInterpreter.nextState(interp)
        }
        println("--- DEBUG Interpreter --- ")
        println(interp)
        println("------ ")
    }

    def stepBack(): Boolean = {
        stateStack match {
            case st::rest => { interp = st; stateStack = rest; true}
            case Nil => {false}
        }
    }



}



object CanvasRenderTest extends SimpleSwingApplication {
    def top = new MainFrame {
        title = "Testing AST visualization"
        val canvas = new DrawingCanvas {
            preferredSize = new Dimension(1000,500)
            background = Color.WHITE
        }
        val textArea = new TextArea("Program"){
            editable = true
            name = "Test # 1"
            text = TestPrograms.test1()
            preferredSize = new Dimension(250, 200)
            font = new Font("Helvetica", Font.PLAIN, 20)
            border = Swing.LineBorder(Color.BLACK)
        }
        val button1 = new Button {
            text = "START"
            borderPainted = true
            enabled = true
            tooltip = "Click and See"
        }
        val button2 = new Button {
            text = "STEP>"
            borderPainted = true
            enabled = false
            tooltip = "Click and See"
        }
        val button3 = new Button {
            text = "<BACK"
            borderPainted = true
            enabled = false
            tooltip = "Click and See"
        }

        listenTo(button1)
        listenTo(button2)
        listenTo(button3)



        def execTextAreaProgram(): Unit = {
            val txt = textArea.text
            println("-------")
            println(txt)
            println("---------")
            val expr = TestPrograms.parseAndInterpretProgram(txt)
            canvas.setAST(expr)
            button3.enabled=false
            canvas.repaint()
        }

        menuBar = new MenuBar {
            contents += new MenuItem(Action("Program1"){
                //execProgram("Program1", TestPrograms.test1)
                textArea.text = TestPrograms.test1()
                execTextAreaProgram()
            })
            contents += new MenuItem(Action("Program2"){
                //execProgram("Program2", TestPrograms.test2)
                textArea.text = TestPrograms.test2()
                execTextAreaProgram()
            })
            contents += new MenuItem(Action("Program3"){
                //execProgram("Program3", TestPrograms.test3)
                textArea.text = TestPrograms.test3()
                execTextAreaProgram()
            })
            contents += new MenuItem(Action("Program4"){
                //execProgram("Program4", TestPrograms.test4)
                textArea.text = TestPrograms.test4()
                execTextAreaProgram()
            })

            contents += new MenuItem(Action("Program5"){
                textArea.text =
                    """
                      |let x = 20 in
                      |   let y = (
                      |          let x = 45 in
                      |              x + 20
                      |   ) in
                      |       x - y * y
                    """.stripMargin
                execTextAreaProgram()
            })

            contents += new MenuItem(Action("XtoGrind"){
                textArea.text =
                  """
                    |let x = 20 in
                    |  let x = 30 in
                    |     let x = x + x * x in
                    |       let y = (
                    |             let x = 45 in
                    |                 x + 20
                    |       ) in
                    |         x - y * y
                  """.stripMargin
                execTextAreaProgram()
            })

        }
        contents = new BoxPanel(Orientation.Vertical) {
            contents += canvas
            contents += textArea
            contents += button1
            contents += button2
            contents += button3
        }

        reactions += {
            case ButtonClicked(b) if (b == button1) => {

                execTextAreaProgram()
                button2.enabled = true

            }
        }
        reactions += {
            case ButtonClicked(b)  if (b == button2) => {
                    println("--- button 2 pressed -- ")
                    canvas.step()
                    canvas.repaint()
                    button3.enabled = true
                }
            }

        reactions +=  {
            case ButtonClicked(b)  if (b == button3) => {
                println("--- button 3 pressed -- ")
                button3.enabled = canvas.stepBack()
                canvas.repaint()
            }
        }

        size = new Dimension(1500,500)

    }
}
