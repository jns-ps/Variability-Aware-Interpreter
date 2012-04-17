package de.puschj.interpreter


object StatementExecutor {
  def execute(stmt: Statement, env: Environment) : Unit = {
    stmt match {
      case Assignment(n,exp) => {
       env.put(n,ExpressionEvaluator.eval(exp,env)) 
       env.print()    
      }
      case Block(stmts) => stmts.foreach(execute(_,env))
      case While(c, s) => while (ConditionEvaluator.eval(c, env)) execute(s, env)
      case If(c, s1, s2) => if (ConditionEvaluator.eval(c, env)) execute(s1, env) else if (s2.isDefined) execute(s2.get, env)
    }
  }
}


object ExpressionEvaluator {
  def eval(exp: Expression, env: Environment) : Int = 
    exp match {
	  case Num(n) => n
	  case Id(x) => env.get(x)
	  case Add(e1,e2) => eval(e1,env) + eval(e2,env)
	  case Sub(e1,e2) => eval(e1,env) - eval(e2,env)
	  case Mul(e1,e2) => eval(e1,env) * eval(e2,env)
	  case Div(e1,e2) => eval(e1,env) / eval(e2,env)
	  case Parens(e) => eval(e, env)
  }
}


object ConditionEvaluator {
  def eval(cnd: Condition, env: Environment) : Boolean = 
    cnd match {
	  case Neg(c) => !(eval(c,env))
	  case GreaterThan(e1,e2) => ExpressionEvaluator.eval(e1,env) > ExpressionEvaluator.eval(e2,env)
	  case LessThan(e1,e2) => ExpressionEvaluator.eval(e1,env) < ExpressionEvaluator.eval(e2,env)
	  case GreaterOrEqualThan(e1,e2) => ExpressionEvaluator.eval(e1,env) >= ExpressionEvaluator.eval(e2,env)
	  case LessOrEqualThan(e1,e2) => ExpressionEvaluator.eval(e1,env) <= ExpressionEvaluator.eval(e2,env)
  }
}