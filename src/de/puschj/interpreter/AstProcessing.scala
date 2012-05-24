package de.puschj.interpreter
import de.fosd.typechef.conditional._
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.featureexpr.FeatureExprFactory
import de.fosd.typechef.featureexpr.FeatureExprFactory.{True, False }

object StatementExecutor {
  def execute(s: Statement, context: FeatureExpr, store: Store): Store = {
    if (context.isContradiction()) return store

    s match {
      case Assignment(key, exp) => {
          store.put(key, Choice(context, ExpressionEvaluator.eval(exp, store), store.get(key)).simplify)
          store.print()
      }
      case Block(stmts) => for (stm <- stmts) execute(stm.entry, stm.feature and context, store)
      case While(c, s) => {
        var isSat: Boolean = true
        while(isSat) {
            val x: FeatureExpr = ConditionEvaluator.whenTrue(c, store)
            isSat = (context and x).isSatisfiable()
            if (isSat) {
              execute(s, context and x, store)
            }
        }
      }
      case If(c, s1, s2) => {
        val x: FeatureExpr = ConditionEvaluator.whenTrue(c, store)
        if ((context and x).isSatisfiable()) {
          execute(s1, context and x, store)
          if (s2.isDefined)
            execute(s2.get, context and x.not(), store)
        }
      }
      case Assert(cnd) => {
        val whenTrue: FeatureExpr = ConditionEvaluator.whenTrue(cnd, store)
        val equivToContext: Boolean = whenTrue.equivalentTo(context)

        if ( !(whenTrue.isTautology() || equivToContext) ) {
          throw new AssertionError("violation of " + cnd +
                                   "\nexpected to be true when: " + renameFeatureExpectation(context) +
                                   "\nactually was true when: " + renameFeatureExpectation(whenTrue))
        } 
      }
    }
    return store;
  }
  
  def renameFeatureExpectation(fe: FeatureExpr): String = {
    if (fe.equivalentTo(FeatureExprFactory.True))
      return "ever"
    if (fe.equivalentTo(FeatureExprFactory.False))
      return "never"
    fe.toString()
  }
}

object ExpressionEvaluator {
  def eval(exp: Expression, store: Store): Conditional[Int] =
    exp match {
      case Num(n) => One(n)
      case Id(x) => store.get(x)
      case Add(e1, e2) => ConditionalLib.mapCombination(eval(e1, store), eval(e2, store), (a: Int, b: Int) => a + b)
      case Sub(e1, e2) => ConditionalLib.mapCombination(eval(e1, store), eval(e2, store), (a: Int, b: Int) => a - b)
      case Mul(e1, e2) => ConditionalLib.mapCombination(eval(e1, store), eval(e2, store), (a: Int, b: Int) => a * b)
      case Div(e1, e2) => ConditionalLib.mapCombination(eval(e1, store), eval(e2, store), (a: Int, b: Int) => a / b)
      case Parens(e) => eval(e, store)
    }
}

object ConditionEvaluator {
  def eval(cnd: Condition, store: Store): Conditional[Boolean] =
    cnd match {
      case Neg(c) => eval(c, store).map(b => !b)
      case Equal(e1, e2) => ConditionalLib.mapCombination(ExpressionEvaluator.eval(e1, store), ExpressionEvaluator.eval(e2, store), (a: Int, b: Int) => a == b)
      case GreaterThan(e1, e2) => ConditionalLib.mapCombination(ExpressionEvaluator.eval(e1, store), ExpressionEvaluator.eval(e2, store), (a: Int, b: Int) => a > b)
      case LessThan(e1, e2) => ConditionalLib.mapCombination(ExpressionEvaluator.eval(e1, store), ExpressionEvaluator.eval(e2, store), (a: Int, b: Int) => a < b)
      case GreaterOrEqualThan(e1, e2) => ConditionalLib.mapCombination(ExpressionEvaluator.eval(e1, store), ExpressionEvaluator.eval(e2, store), (a: Int, b: Int) => a >= b)
      case LessOrEqualThan(e1, e2) => ConditionalLib.mapCombination(ExpressionEvaluator.eval(e1, store), ExpressionEvaluator.eval(e2, store), (a: Int, b: Int) => a <= b)
    }

  def whenTrue(c: Condition, store: Store): FeatureExpr = whenTrueRek(True, eval(c, store))

  private def whenTrueRek(fe: FeatureExpr, c: Conditional[Boolean]): FeatureExpr = {
    c match {
      case One(x) => if (x) fe else False
      case Choice(feature, thn, els) =>
        whenTrueRek(feature and fe, thn) or whenTrueRek(feature.not() and fe, els)
    }
  }
}