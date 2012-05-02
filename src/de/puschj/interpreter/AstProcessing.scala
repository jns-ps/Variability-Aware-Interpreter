package de.puschj.interpreter
import de.fosd.typechef.conditional._
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.featureexpr.FeatureExprFactory.True



object StatementExecutor {
  def execute(s: Opt[Statement], env: Environment) : Environment = {
    val feature: FeatureExpr = s.feature
    if (feature.isContradiction()) return env
    
    s.entry match {
      case Assignment(key ,exp) => {
        if (feature.isTautology()) 
          env.put(key, ExpressionEvaluator.eval(exp, env))
        else
          env.put(key, Choice(feature, ExpressionEvaluator.eval(exp, env), env.get(key)))
        
        env.print()
      }
      case Block(stmts) => stmts.foreach(execute(_,env))
//      case While(c, s) => while (ConditionEvaluator.eval(c, env)) execute(s, env)
//      case If(c, s1, s2) => if (ConditionEvaluator.eval(c, env)) execute(s1, env) else if (s2.isDefined) execute(s2.get, env)
    
    }
    return env;
  }
}


object ExpressionEvaluator {
  def eval(exp: Expression, env: Environment) : Conditional[Int] = 
    exp match {
          case Num(n) => One(n)
		  case Id(x) => env.get(x)
		  case Add(e1,e2) => calc(eval(e1,env), eval(e2,env), (a, b) => a + b)
		  case Sub(e1,e2) => calc(eval(e1,env), eval(e2,env), (a, b) => a - b)
		  case Mul(e1,e2) => calc(eval(e1,env), eval(e2,env), (a, b) => a * b)
		  case Div(e1,e2) => calc(eval(e1,env), eval(e2,env), (a, b) => a / b)
		  case Parens(e) => eval(e, env)
    }
  
  
  def calc(a: Conditional[Int], b: Conditional[Int], f: (Int, Int) => Int): Conditional[Int] = {
	a match {
	  case One(x) => b.map( n => f(x,n) )
	  case Choice(feature, thn, els) => {
	    Choice(feature, calc(thn, b, f), calc(els, b, f)).simplify
	  }
	}
  }
}


object ConditionEvaluator {
  def eval(cnd: Condition, env: Environment) : Conditional[Boolean] = 
    cnd match {
	  case Neg(c) => eval(c,env).map( b => !b )
	  case Equal(e1,e2) => compare(ExpressionEvaluator.eval(e1,env), ExpressionEvaluator.eval(e2,env), (a, b) => a == b)
	  case GreaterThan(e1,e2) => compare(ExpressionEvaluator.eval(e1,env), ExpressionEvaluator.eval(e2,env), (a, b) => a > b)
	  case LessThan(e1,e2) => compare(ExpressionEvaluator.eval(e1,env), ExpressionEvaluator.eval(e2,env), (a, b) => a < b)
	  case GreaterOrEqualThan(e1,e2) => compare(ExpressionEvaluator.eval(e1,env), ExpressionEvaluator.eval(e2,env), (a, b) => a >= b)
	  case LessOrEqualThan(e1,e2) => compare(ExpressionEvaluator.eval(e1,env), ExpressionEvaluator.eval(e2,env), (a, b) => a <= b)
  }
  
  def compare(a: Conditional[Int], b: Conditional[Int], f: (Int, Int) => Boolean): Conditional[Boolean] = {
    a match {
      case One(x) => b.map( n => f(x,n) )
      case Choice(feature, thn, els) => {
        Choice(feature, compare(thn, b, f), compare(els, b, f)).simplify
      }
    }
  }
}