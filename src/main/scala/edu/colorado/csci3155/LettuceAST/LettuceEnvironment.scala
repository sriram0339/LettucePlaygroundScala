package LettuceAST

trait LettuceEnvironment {
    def lookup(s: String): Value
}

case object EmptyEnvironment extends LettuceEnvironment {
    override def lookup(s:String): Value = throw new UnboundIdentifierError(s"Identifier $s is not known")
}
case class ExtendEnv(id: String, v: Value, e: LettuceEnvironment) extends LettuceEnvironment {
    override def lookup(s: String): Value = {
        if (id == s){
            v
        } else {
            e.lookup(s)
        }
    }
}


case class ExtendEnvRec(f: String, param: String, e: Expr,  eHat: LettuceEnvironment) extends LettuceEnvironment {
    override def lookup(s: String): Value = {
        if (s == f){
            Closure(param, e, this)
        } else {
            eHat.lookup(s)
        }
    }
}


