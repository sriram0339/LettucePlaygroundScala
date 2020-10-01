package LettuceAST {

    case class TypeConversionError(s: String) extends Exception {
        override def toString: String = {
            s"Type Conversion Error: $s"
        }
    }

    case class UnboundIdentifierError(s: String) extends Exception {
        override def toString: String = {
            s"Unknown Identifier: $s"
        }
    }

    case class RuntimeError(s: String) extends Exception {
        override def toString: String = {
            s"Runtime Error: $s"
        }
    }

    case class SyntaxError(s: String) extends Exception {
        override def toString: String = {
            s"Syntax Error: $s"
        }
    }

}
