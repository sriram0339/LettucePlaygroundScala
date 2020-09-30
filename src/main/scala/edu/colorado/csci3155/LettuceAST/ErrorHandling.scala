package LettuceAST {

    case class TypeConversionError(s: String) extends Exception

    case class UnboundIdentifierError(s: String) extends Exception

    case class RuntimeError(s: String) extends Exception

    case class SyntaxError(s: String) extends Exception

}
