package LettuceAST


/*
   A list of test programs that we can test on.
   These programs are added to the main window of the application when we run it.
   Author: Sriram Sankaranarayanan <srirams@colorado>
*/

object TestPrograms {
    val debug = true
    def test1(): String =
        """
          |let f = 20 in
          |   let g = f - 35.5 in
          |      f/g
        """.stripMargin

    def test2(): String =
        """
          |let y = 200 in
          |let f = function(x)
          |         ( x + 25.0 )
          |         in
          |let g = function (y) (f(y)*f(y)/f(y)) in
          |       g(3)
        """.stripMargin

    def test3(): String =
        """
          | let z0 = 20 in
          |   let z1 = (let z0 = 35 in
          |              if (z0 <= 25)
          |              then z0 - 2
          |              else z0 + 3 ) in
          |     let z2 = z0 + 2 * z1 in
          | z2
        """.stripMargin


    def test4(): String = {
        """
          |letrec fact = function (x)
          |       if (x <= 1)
          |       then 1
          |       else  fact (x-1) * x
          |      in
          |   fact(10)
        """.stripMargin
    }

    def yCombTest(): String = {
        """
          |let Y = function (f)  (
          |             let f1 = function (g) ( f ( function (v) ( g(g)(v) )  )  ) in
          |             let f2 = function(g) ( f ( function(v) g(g)(v) ) ) in
          |               f1(f2)
          |              ) in
          |   let F = function (f) ( function(x) ( if (x == 0) then 1 else x * f(x-1) ) ) in
          |        Y(F)(3)
          |
          |""".stripMargin
    }



    def parseAndInterpretProgram(s: String): Expr= {
            val p: Program = new LettuceParser().parseString(s)
            if (debug) {
                println("--- Debug --- ")
                println(p)
                println("--- Debug ---")
            }
            p match {
                case TopLevel(e) => e
                case _ => throw new SyntaxError("Could not parse the program -- fatal error")
            }
    }


}

