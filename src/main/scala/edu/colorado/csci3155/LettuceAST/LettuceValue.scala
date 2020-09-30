package LettuceAST


trait Value

case class NumValue(f: Double) extends Value
case class BoolValue(b: Boolean) extends Value
case class Closure(arg: String, e: Expr, sigma: LettuceEnvironment) extends Value

object  LettuceValue {

    def valueToNum(v: Value): Double =  v match {
        case NumValue(f) => f
        case _ =>  throw new TypeConversionError("Converting from non numeric to number value")
    }

    def valueToBool(v: Value): Boolean = v match  {
        case BoolValue(b) => b
        case _ => throw new TypeConversionError("Converting from non boolean to boolean value")
    }

    def valueToClosure(v: Value): Closure = v match  {
        case Closure(aList, e, sigma) => Closure(aList, e, sigma)
        case _ => throw new TypeConversionError("Converting from non closure to a closure value")
    }


    def plus (v1: Value) (v2: Value) : Value = (v1, v2) match {
        case  (NumValue(f1), NumValue(f2)) =>  NumValue(f1 + f2)
        case _ => throw new TypeConversionError("Converting from non numeric to number value")
    }

    def minus (v1: Value) (v2: Value) : Value = (v1, v2) match {
        case  (NumValue(f1), NumValue(f2)) =>  NumValue(f1 - f2)
        case _ => throw new TypeConversionError("Converting from non numeric to number value")
    }

    def mult (v1: Value) (v2: Value) : Value = (v1, v2) match {
        case  (NumValue(f1), NumValue(f2)) =>  NumValue(f1 * f2)
        case _ => throw new TypeConversionError("Converting from non numeric to number value")
    }

    def div (v1: Value) (v2: Value) : Value = (v1, v2) match {
        case  (NumValue(f1), NumValue(f2)) if f2 != 0.0 =>  NumValue(f1 / f2)
        case  (NumValue(_), NumValue(f2)) if f2 == 0.0 => throw new RuntimeError("Division by zero!")
        case _ => throw new TypeConversionError("Converting from non numeric to number value")
    }

    def geq (v1: Value) (v2: Value) : Value = (v1, v2) match {
        case  (NumValue(f1), NumValue(f2)) =>  BoolValue(f1 >= f2)
        case _ => throw new TypeConversionError("Converting from non numeric to number value")
    }

    def gt (v1: Value) (v2: Value) : Value = (v1, v2) match {
        case  (NumValue(f1), NumValue(f2)) =>  BoolValue(f1 > f2)
        case _ => throw new TypeConversionError("Converting from non numeric to number value")
    }

    def eq (v1: Value) (v2: Value) : Value = (v1, v2) match {
        case  (NumValue(f1), NumValue(f2)) =>  BoolValue(f1 == f2)
        case (BoolValue(b1), BoolValue(b2)) => BoolValue(b1 == b2)
        case _ => throw new TypeConversionError("Converting from non numeric to number value")
    }

    def neq (v1: Value) (v2: Value) : Value = (v1, v2) match {
        case  (NumValue(f1), NumValue(f2)) =>  BoolValue(f1 != f2)
        case (BoolValue(b1), BoolValue(b2)) => BoolValue(b1 != b2)
        case _ => throw new TypeConversionError("Converting from non numeric to number value")
    }

    def log(v: Value): Value = v match {
        case NumValue(f) => NumValue(Math.log(f))
        case _ => throw new TypeConversionError("Converting from non numeric to number value")
    }

    def exp(v: Value): Value = v match {
        case NumValue(f) => NumValue(Math.exp(f))
        case _ => throw new TypeConversionError("Converting from non numeric to number value")
    }

    def sine(v: Value): Value = v match {
        case NumValue(f) => NumValue(Math.sin(f))
        case _ => throw new TypeConversionError("Converting from non numeric to number value")
    }
    def cosine(v: Value): Value = v match {
        case NumValue(f) => NumValue(Math.cos(f))
        case _ => throw new TypeConversionError("Converting from non numeric to number value")
    }

    def not (v: Value) : Value = v match {
        case BoolValue(b) => BoolValue(!b)
        case _ => throw new TypeConversionError("Converting from non boolean to boolean value")
    }




}