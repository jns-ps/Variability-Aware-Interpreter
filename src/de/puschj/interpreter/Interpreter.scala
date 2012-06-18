package de.puschj.interpreter

import de.fosd.typechef.conditional._
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.featureexpr.FeatureExprFactory
import de.fosd.typechef.featureexpr.FeatureExprFactory.{True, False }


object Interpreter {
  
  @throws(classOf[LoopExceededException])
  def execute(s: Statement, context: FeatureExpr, store: Store, funcStore: FuncStore): Unit = {
    if (context.isContradiction()) return
    s match {
      case Assignment(key, exp) => store.put(key, Choice(context, eval(exp, store, funcStore), store.get(key)).simplify)
      case Block(stmts) => for (stm <- stmts) execute(stm.entry, stm.feature and context, store, funcStore)
      case w@While(c, s) => {
        var isSat: Boolean = true
        var n = 0
        while(isSat && (n < 100)) {
            val x: FeatureExpr = whenTrue(c, store, funcStore)
            isSat = (context and x).isSatisfiable
            if (isSat)
              execute(s, context and x, store, funcStore)
            n += 1
        }
        if ( n >= 100 ) {
           throw new LoopExceededException("Exceeded Loop in Statement: " + w)
        }
      }
      case If(c, s1, s2) => {
        val x: FeatureExpr = whenTrue(c, store, funcStore)
        if (context.isSatisfiable) {
            if (x.isSatisfiable)
                execute(s1, context and x, store, funcStore)
            if (s2.isDefined)
                execute(s2.get, context andNot x, store, funcStore)
        }
      }
      case Assert(cnd) => {
        val whentrue: FeatureExpr = whenTrue(cnd, store, funcStore)
        val equivToContext: Boolean = whentrue.equivalentTo(context)
        if ( !(whentrue.isTautology || equivToContext) ) {
          throw new AssertionError("violation of " + cnd +
                                   "\nexpected to be true when: " + renameFeatureExpectation(context) +
                                   "\nactually was true when: " + renameFeatureExpectation(whentrue))
        } 
      }
      case FuncDef(name, args, body) => {
        funcStore.put(name, FunctionDef(args, body))
      }
    }
  }
  
  def renameFeatureExpectation(fe: FeatureExpr): String = {
    if (fe.equivalentTo(FeatureExprFactory.True))
      return "ever"
    if (fe.equivalentTo(FeatureExprFactory.False))
      return "never"
    fe.toString()
  }

  def eval(exp: Expression, store: Store, funcStore: FuncStore): Conditional[Value] =
    exp match {
      // arithmetic
      case Num(n) => One(IntValue(n))
      case Id(x) => store.get(x)
      case Add(e1, e2) => ConditionalLib.mapCombination(eval(e1, store, funcStore), eval(e2, store, funcStore), 
          (a: Value, b: Value) => calculateValue(a, b, (a,b) => IntValue(a+b) ))    
      case Sub(e1, e2) => ConditionalLib.mapCombination(eval(e1, store, funcStore), eval(e2, store, funcStore), 
          (a: Value, b: Value) => calculateValue(a, b, (a,b) => IntValue(a-b) ))
      case Mul(e1, e2) => ConditionalLib.mapCombination(eval(e1, store, funcStore), eval(e2, store, funcStore), 
          (a: Value, b: Value) => calculateValue(a, b, (a,b) => IntValue(a*b) ))
      case Div(e1, e2) => ConditionalLib.mapCombination(eval(e1, store, funcStore), eval(e2, store, funcStore), 
          (a: Value, b: Value) => calculateValue(a, b, 
              (a,b) => if (b==0) NotANumberValue("divide by zero") else IntValue(a/b) ))
      case Parens(e) => eval(e, store, funcStore)
      // conditions
      case Neg(c) => eval(c, store, funcStore).map(value => {
        value match {
          case u@UndefinedValue(_) => u
          case v => BoolValue(!v.getBoolValue())
        }
      })
      case Equal(e1, e2) => ConditionalLib.mapCombination(eval(e1, store, funcStore), eval(e2, store, funcStore), 
          (a: Value, b: Value) => calculateValue(a, b, (a,b) => BoolValue(a==b)))
      case GreaterThan(e1, e2) => ConditionalLib.mapCombination(eval(e1, store, funcStore), eval(e2, store, funcStore), 
          (a: Value, b: Value) => calculateValue(a, b, (a,b) => BoolValue(a>b)))
      case LessThan(e1, e2) => ConditionalLib.mapCombination(eval(e1, store, funcStore), eval(e2, store, funcStore), 
          (a: Value, b: Value) => calculateValue(a, b, (a,b) => BoolValue(a<b)))
      case GreaterOE(e1, e2) => ConditionalLib.mapCombination(eval(e1, store, funcStore), eval(e2, store, funcStore), 
          (a: Value, b: Value) => calculateValue(a, b, (a,b) => BoolValue(a>=b)))
      case LessOE(e1, e2) => ConditionalLib.mapCombination(eval(e1, store, funcStore), eval(e2, store, funcStore), 
          (a: Value, b: Value) => calculateValue(a, b, (a,b) => BoolValue(a<=b)))
      // functions
      case Call(name, args) => {
        val fdef = funcStore.get(name)
        val vals = args.map(eval(_, store, funcStore))
        val nArgs = fdef.args.size
        if (nArgs != vals.size) throw new RuntimeException("Illegal number of arguments.")
        val staticScopeStore = new Store()
        for (i <- 0 until nArgs)
          staticScopeStore.put(fdef.args(i), vals(i))
        execute(fdef.body, True, staticScopeStore, funcStore)
        return staticScopeStore.get("res")
      }
    }
  
  private def calculateValue(a: Value, b: Value, f: (Int, Int) => Value) = {
    (a, b) match {
      case (ErrorValue(s1), ErrorValue(s2)) => ErrorValue("multiple errors") //(s1+";"+s2) 
      case (e: ErrorValue, _) => e
      case (_, e: ErrorValue) => e
      case (a, b) => f(a.getIntValue(), b.getIntValue())
    }
  }

  def whenTrue(c: Condition, store: Store, funcStore: FuncStore): FeatureExpr = whenTrueRek(True, eval(c, store, funcStore))

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