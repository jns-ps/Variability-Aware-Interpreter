package de.puschj.interpreter

import de.fosd.typechef.conditional._
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.featureexpr.FeatureExprFactory
import de.fosd.typechef.featureexpr.FeatureExprFactory.{True, False }


object Interpreter {
  
  def execute(s: Statement, context: FeatureExpr, store: Store): Store = {
    if (context.isContradiction()) return store

    s match {
      case Assignment(key, exp) => {
          store.put(key, Choice(context, eval(exp, store), store.get(key)).simplify)
//          store.print()
      }
      case Block(stmts) => for (stm <- stmts) execute(stm.entry, stm.feature and context, store)
      case While(c, s) => {
        var isSat: Boolean = true
        var n = 0
        while(isSat && (n < 100)) {
            val x: FeatureExpr = whenTrue(c, store)
            isSat = (context and x).isSatisfiable()
            if (isSat) {
              execute(s, context and x, store)
            }
            n += 1
        }
      }
      case If(c, s1, s2) => {
        val x: FeatureExpr = whenTrue(c, store)
        if ((context and x).isSatisfiable()) {
          execute(s1, context and x, store)
          if (s2.isDefined)
            execute(s2.get, context and x.not(), store)
        }
      }
      case Assert(cnd) => {
        val whentrue: FeatureExpr = whenTrue(cnd, store)
        val equivToContext: Boolean = whentrue.equivalentTo(context)
        if ( !(whentrue.isTautology() || equivToContext) ) {
          throw new AssertionError("violation of " + cnd +
                                   "\nexpected to be true when: " + renameFeatureExpectation(context) +
                                   "\nactually was true when: " + renameFeatureExpectation(whentrue))
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

  def eval(exp: Expression, store: Store): Conditional[Value] =
    exp match {
      // arithmetic
      case Num(n) => One(IntValue(n))
      case Id(x) => store.get(x)
      case Add(e1, e2) => ConditionalLib.mapCombination(eval(e1, store), eval(e2, store), 
          (a: Value, b: Value) => calculateValue(a, b, (a,b) => IntValue(a+b) ))    
      case Sub(e1, e2) => ConditionalLib.mapCombination(eval(e1, store), eval(e2, store), 
          (a: Value, b: Value) => calculateValue(a, b, (a,b) => IntValue(a-b) ))
      case Mul(e1, e2) => ConditionalLib.mapCombination(eval(e1, store), eval(e2, store), 
          (a: Value, b: Value) => calculateValue(a, b, (a,b) => IntValue(a*b) ))
      case Div(e1, e2) => ConditionalLib.mapCombination(eval(e1, store), eval(e2, store), 
          (a: Value, b: Value) => calculateValue(a, b, 
              (a,b) => if (b==0) NotANumberValue("divide by zero") else IntValue(a/b) ))
      case Parens(e) => eval(e, store)
      // conditions
      case Neg(c) => eval(c, store).map(value => {
        value match {
          case u@UndefinedValue(_) => u
          case v => BoolValue(!v.getBoolValue())
        }
      })
      case Equal(e1, e2) => ConditionalLib.mapCombination(eval(e1, store), eval(e2, store), 
          (a: Value, b: Value) => calculateValue(a, b, (a,b) => BoolValue(a==b)))
      case GreaterThan(e1, e2) => ConditionalLib.mapCombination(eval(e1, store), eval(e2, store), 
          (a: Value, b: Value) => calculateValue(a, b, (a,b) => BoolValue(a>b)))
      case LessThan(e1, e2) => ConditionalLib.mapCombination(eval(e1, store), eval(e2, store), 
          (a: Value, b: Value) => calculateValue(a, b, (a,b) => BoolValue(a<b)))
      case GreaterOE(e1, e2) => ConditionalLib.mapCombination(eval(e1, store), eval(e2, store), 
          (a: Value, b: Value) => calculateValue(a, b, (a,b) => BoolValue(a>=b)))
      case LessOE(e1, e2) => ConditionalLib.mapCombination(eval(e1, store), eval(e2, store), 
          (a: Value, b: Value) => calculateValue(a, b, (a,b) => BoolValue(a<=b)))
    }
  
  private def calculateValue(a: Value, b: Value, f: (Int, Int) => Value) = {
    (a, b) match {
      case (e1: ErrorValue, e2: ErrorValue) => ErrorValue("multiple errors") //s1+"/"+s2
      case (e: ErrorValue, _) => e
      case (_, e: ErrorValue) => e
      case (a, b) => f(a.getIntValue(), b.getIntValue())
    }
  }

  def whenTrue(c: Condition, store: Store): FeatureExpr = whenTrueRek(True, eval(c, store))

  private def whenTrueRek(fe: FeatureExpr, c: Conditional[Value]): FeatureExpr = {
    c match {
      case One(value) => {
        value match {
          case ErrorValue(_) => False
          case x => if (x.getBoolValue()) fe else False
        }
      }
      case Choice(feature, thn, els) =>
        whenTrueRek(feature and fe, thn) or whenTrueRek(feature.not() and fe, els)
    }
  }
}