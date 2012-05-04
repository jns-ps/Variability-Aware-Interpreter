package de.puschj.interpreter
import de.fosd.typechef.conditional._
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.featureexpr.FeatureExprFactory
import de.fosd.typechef.featureexpr.FeatureExprFactory.{True, False }

object StatementExecutor {
  def execute(s: Statement, fe: FeatureExpr, env: Environment): Environment = {
    if (fe.isContradiction()) return env

    s match {
      case Assignment(key, exp) => {
        if (fe.isTautology())
          env.put(key, ExpressionEvaluator.eval(exp, env))
        else
          env.put(key, Choice(fe, ExpressionEvaluator.eval(exp, env), env.get(key)).simplify)

//        env.print()
      }
      case Block(stmts) => for (stm <- stmts) execute(stm.entry, stm.feature and fe, env)
      case While(c, s) => {
        var isSat: Boolean = true
        while(isSat) {
            val x: FeatureExpr = ConditionEvaluator.whenTrue(c, env)
            isSat = x.isSatisfiable()
            if (isSat) {
              execute(s, x, env)
            }
        }
      }
      case If(c, s1, s2) => {
        val x: FeatureExpr = ConditionEvaluator.whenTrue(c, env)
        if (x.isSatisfiable()) {
          execute(s1, x, env)
          if (s2.isDefined)
            execute(s2.get, x.not(), env)
        }
      }
      case Assert(cnd) => {
        val whenTrue: FeatureExpr = ConditionEvaluator.whenTrue(cnd, env)
        val equivWithCurrent: Boolean = whenTrue.equivalentTo(fe)
        if ( !(whenTrue.isTautology() || equivWithCurrent) )
          throw new AssertionError("violation of "+cnd)     
      }
    }
    return env;
  }
}

object ExpressionEvaluator {
  def eval(exp: Expression, env: Environment): Conditional[Int] =
    exp match {
      case Num(n) => One(n)
      case Id(x) => env.get(x)
      case Add(e1, e2) => ConditionalLib.mapCombination(eval(e1, env), eval(e2, env), (a: Int, b: Int) => a + b)
      case Sub(e1, e2) => ConditionalLib.mapCombination(eval(e1, env), eval(e2, env), (a: Int, b: Int) => a - b)
      case Mul(e1, e2) => ConditionalLib.mapCombination(eval(e1, env), eval(e2, env), (a: Int, b: Int) => a * b)
      case Div(e1, e2) => ConditionalLib.mapCombination(eval(e1, env), eval(e2, env), (a: Int, b: Int) => a / b)
      case Parens(e) => eval(e, env)
    }
}

object ConditionEvaluator {
  def eval(cnd: Condition, env: Environment): Conditional[Boolean] =
    cnd match {
      case Neg(c) => eval(c, env).map(b => !b)
      case Equal(e1, e2) => ConditionalLib.mapCombination(ExpressionEvaluator.eval(e1, env), ExpressionEvaluator.eval(e2, env), (a: Int, b: Int) => a == b)
      case GreaterThan(e1, e2) => ConditionalLib.mapCombination(ExpressionEvaluator.eval(e1, env), ExpressionEvaluator.eval(e2, env), (a: Int, b: Int) => a > b)
      case LessThan(e1, e2) => ConditionalLib.mapCombination(ExpressionEvaluator.eval(e1, env), ExpressionEvaluator.eval(e2, env), (a: Int, b: Int) => a < b)
      case GreaterOrEqualThan(e1, e2) => ConditionalLib.mapCombination(ExpressionEvaluator.eval(e1, env), ExpressionEvaluator.eval(e2, env), (a: Int, b: Int) => a >= b)
      case LessOrEqualThan(e1, e2) => ConditionalLib.mapCombination(ExpressionEvaluator.eval(e1, env), ExpressionEvaluator.eval(e2, env), (a: Int, b: Int) => a <= b)
    }

  def whenTrue(c: Condition, env: Environment): FeatureExpr = whenTrueRek(True, eval(c, env))

  private def whenTrueRek(fe: FeatureExpr, c: Conditional[Boolean]): FeatureExpr = {
    c match {
      case One(x) => if (x) fe else False
      case Choice(feature, thn, els) =>
        whenTrueRek(feature and fe, thn) or whenTrueRek(feature.not() and fe, els)
    }
  }
}