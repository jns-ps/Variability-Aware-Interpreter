package de.puschj.interpreter

import de.fosd.typechef.conditional._
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.featureexpr.FeatureExprFactory
import de.fosd.typechef.featureexpr.FeatureExprFactory.{ True, False }

object VAInterpreter {

  @throws(classOf[LoopExceededException])
  def execute(s: Statement, context: FeatureExpr, store: VAStore, funcStore: VAFuncStore, classStore: VAClassStore): Unit = {
    if (context.isContradiction()) return
    s match {
      case Assignment(key, exp) => store.put(key, Choice(context, eval(exp, store, funcStore, classStore), store.get(key)).simplify)
      case Block(stmts) => for (stm <- stmts) execute(stm.entry, stm.feature and context, store, funcStore, classStore)
      case w @ While(c, s) => {
        var isSat: Boolean = true
        var n = 0
        while (isSat && (n < 100)) {
          val x: FeatureExpr = whenTrue(c, store, funcStore, classStore)
          isSat = (context and x).isSatisfiable
          if (isSat)
            execute(s, context and x, store, funcStore, classStore)
          n += 1
        }
        if (n >= 100) {
          throw new LoopExceededException("Exceeded Loop in Statement: " + w)
        }
      }
      case If(c, s1, s2) => {
        val x: FeatureExpr = whenTrue(c, store, funcStore, classStore)
        if (context.isSatisfiable) {
          if (x.isSatisfiable)
            execute(s1, context and x, store, funcStore, classStore)
          if (s2.isDefined)
            execute(s2.get, context andNot x, store, funcStore, classStore)
        }
      }
      case Assert(cnd) => {
        val whentrue: FeatureExpr = whenTrue(cnd, store, funcStore, classStore)
        val equivToContext: Boolean = whentrue.equivalentTo(context)
        if (!(whentrue.isTautology || equivToContext)) {
          throw new AssertionError("violation of " + cnd +
            "\nexpected to be true when: " + context +
            "\nactually was true when: " + whentrue)
        }
      }
      case FuncDec(name, args, body) => {
        funcStore.put(name, Choice(context, One(FDef(args, body)), funcStore.get(name)).simplify)
      }
      case ClassDec(name, superClass, fields, optFuncDecs) => {
          val funcStore = new VAFuncStore
          for (optFuncDec <- optFuncDecs) {
              val funcDec = optFuncDec.entry
              funcStore.put(funcDec.name, Choice(optFuncDec.feature, One(FDef(funcDec.args, funcDec.body)), funcStore.get(funcDec.name)).simplify)
          }
          classStore.put(name, Choice(context, One(CDef(superClass, fields, funcStore)), classStore.get(name)).simplify)
      }
    }
  }

  private def eval(exp: Expression, store: VAStore, funcStore: VAFuncStore, classStore: VAClassStore): Conditional[Value] =
    exp match {
      // arithmetic
      case Num(n) => One(IntValue(n))
      case Id(x) => store.get(x)
      case Add(e1, e2) => ConditionalLib.mapCombination(eval(e1, store, funcStore, classStore), eval(e2, store, funcStore, classStore),
        (a: Value, b: Value) => calculateValue(a, b, (a, b) => IntValue(a + b)))
      case Sub(e1, e2) => ConditionalLib.mapCombination(eval(e1, store, funcStore, classStore), eval(e2, store, funcStore, classStore),
        (a: Value, b: Value) => calculateValue(a, b, (a, b) => IntValue(a - b)))
      case Mul(e1, e2) => ConditionalLib.mapCombination(eval(e1, store, funcStore, classStore), eval(e2, store, funcStore, classStore),
        (a: Value, b: Value) => calculateValue(a, b, (a, b) => IntValue(a * b)))
      case Div(e1, e2) => ConditionalLib.mapCombination(eval(e1, store, funcStore, classStore), eval(e2, store, funcStore, classStore),
        (a: Value, b: Value) => calculateValue(a, b,
          (a, b) => if (b == 0) UndefinedValue("divide by zero") else IntValue(a / b)))
      case Parens(e) => eval(e, store, funcStore, classStore)
      // conditions
      case Neg(c) => eval(c, store, funcStore, classStore).map(value => {
        value match {
          case e @ ErrorValue(_) => e
          case v => BoolValue(!v.getBoolValue())
        }
      })
      case Equal(e1, e2) => ConditionalLib.mapCombination(eval(e1, store, funcStore, classStore), eval(e2, store, funcStore, classStore),
        (a: Value, b: Value) => calculateValue(a, b, (a, b) => BoolValue(a == b)))
      case GreaterThan(e1, e2) => ConditionalLib.mapCombination(eval(e1, store, funcStore, classStore), eval(e2, store, funcStore, classStore),
        (a: Value, b: Value) => calculateValue(a, b, (a, b) => BoolValue(a > b)))
      case LessThan(e1, e2) => ConditionalLib.mapCombination(eval(e1, store, funcStore, classStore), eval(e2, store, funcStore, classStore),
        (a: Value, b: Value) => calculateValue(a, b, (a, b) => BoolValue(a < b)))
      case GreaterOE(e1, e2) => ConditionalLib.mapCombination(eval(e1, store, funcStore, classStore), eval(e2, store, funcStore, classStore),
        (a: Value, b: Value) => calculateValue(a, b, (a, b) => BoolValue(a >= b)))
      case LessOE(e1, e2) => ConditionalLib.mapCombination(eval(e1, store, funcStore, classStore), eval(e2, store, funcStore, classStore),
        (a: Value, b: Value) => calculateValue(a, b, (a, b) => BoolValue(a <= b)))
      // functions
      case Call(name, args) => {
        funcStore.get(name).mapr(fdef => fdef match {
          case FErr(msg) => One(UndefinedValue(msg))
          case FDef(fargs, fbody) => {
            val nArgs = fargs.size
            if (nArgs != args.size)
              throw new RuntimeException("Illegal number of arguments.")
            val vals = args.map(a => Choice(a.feature, eval(a.entry, store, funcStore, classStore), One(UndefinedValue("undef func arg"))))
            val staticScopeStore = new VAStore()
            for (i <- 0 until nArgs)
              staticScopeStore.put(fargs(i), vals(i))
            execute(fbody, True, staticScopeStore, funcStore, classStore)
            staticScopeStore.get("res")
          }
        })
      }
      case New(name, args) => {
        classStore.get(name).map(cdef => cdef match {
          case CErr(msg) => UndefinedValue(msg)
          case CDef(superClass, fields, methods) => {
            if (fields.size != args.size)
              throw new RuntimeException("Illegal number of construction arguments.")
            val vals = args.map(a => Choice(a.feature, eval(a.entry, store, funcStore, classStore), One(UndefinedValue("undef constructor arg"))))
            val objectStore = new VAStore
            
            for (i <- 0 until fields.size)
                objectStore.put(fields(i).entry, vals(i))
            VAObjectValue(name, objectStore)
          }
        })
      }
      case Field(expr, name) => {
        eval(expr, store, funcStore, classStore).mapr(v => v match {
          case e: ErrorValue => One(e)
          case o: VAObjectValue => o.getFieldValue(name)
          case x => throw new RuntimeException("cannot get field of non object Value")
        })
      }
      case MethodCall(expr, call) => {
        eval(expr, store, funcStore, classStore).mapr(v => v match {
          case e: ErrorValue => One(e)
          case VAObjectValue(cName, vars) => {
            // TODO: implement
            // different scope
            // get FuncDef from ClassStore
            val clazz = classStore.get(cName)
            val funcDef = clazz.mapr(_ match {
              case CDef(_, _, funcStore) => funcStore.get(call.fname)
              case e: CErr => One(FErr("class '"+cName+"' not defined"))
            })
            funcDef.mapr(fdef => fdef match {
              case FErr(msg) => One(UndefinedValue(msg))
              case FDef(fargs, fbody) => {
                val nArgs = fargs.size
                if (nArgs != call.args.size)
                  throw new RuntimeException("Illegal number of arguments.")
                val vals = call.args.map(a => Choice(a.feature, eval(a.entry, store, funcStore, classStore), One(UndefinedValue("undef func arg"))))
                vars.remove("res")
                for (i <- 0 until nArgs) {
                   if (vars.contains(fargs(i)))
                     throw new RuntimeException("local variable '"+fargs(i)+"' overrides global variable")
                   vars.put(fargs(i), vals(i))
                }
                execute(fbody, True, vars, funcStore, classStore)
                val result = vars.get("res")
                for (i <- 0 until nArgs) {
                  vars.remove(fargs(i))
                }
                result
              }
            })
            
          }
          case x => throw new RuntimeException("cannot invoke method on: '"+x.getClass.getCanonicalName+"'")
        })
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

  private def whenTrue(c: Condition, store: VAStore, funcStore: VAFuncStore, classStore: VAClassStore): FeatureExpr =
    eval(c, store, funcStore, classStore).when(_ match {
      case ErrorValue(_) => false
      case value => value.getBoolValue
    })
}

object PlainInterpreter {

  @throws(classOf[LoopExceededException])
  def execute(s: Statement, store: PlainStore, funcStore: PlainFuncStore, classStore: PlainClassStore): Unit = {
    s match {
      case Assignment(key, exp) => store.put(key, eval(exp, store, funcStore, classStore))
      case Block(stmts) => for (stm <- stmts) execute(stm.entry, store, funcStore, classStore)
      case w @ While(c, s) => {
        var n = 0
        var cnd = true
        while (cnd && (n < 100)) {
          cnd = eval(c, store, funcStore, classStore) match {
            case ErrorValue(_) => false
            case v => v.getBoolValue
          }
          if (cnd)
            execute(s, store, funcStore, classStore)
          n += 1
        }
        if (n >= 100) {
          throw new LoopExceededException("Exceeded Loop in Statement: " + w)
        }
      }
      case If(c, s1, s2) => {
        val cnd = eval(c, store, funcStore, classStore)
        if (!cnd.isInstanceOf[ErrorValue] && cnd.getBoolValue)
          execute(s1, store, funcStore, classStore)
        else if (s2.isDefined)
          execute(s2.get, store, funcStore, classStore)

      }
      case Assert(c) => {
        val cnd = eval(c, store, funcStore, classStore)
        if (cnd.isInstanceOf[ErrorValue] || !cnd.getBoolValue)
          throw new AssertionError("violation of " + cnd)
      }
      case FuncDec(name, args, body) => {
          funcStore.put(name, FDef(args, body))
      }
    }
  }

  private def eval(exp: Expression, store: PlainStore, funcStore: PlainFuncStore, classStore: PlainClassStore): Value =
    exp match {
      // arithmetic
      case Num(n) => IntValue(n)
      case Id(x) => store.get(x)
      case Add(e1, e2) => calculateValue(eval(e1, store, funcStore, classStore), eval(e2, store, funcStore, classStore), (a, b) => IntValue(a + b))
      case Sub(e1, e2) => calculateValue(eval(e1, store, funcStore, classStore), eval(e2, store, funcStore, classStore), (a, b) => IntValue(a - b))
      case Mul(e1, e2) => calculateValue(eval(e1, store, funcStore, classStore), eval(e2, store, funcStore, classStore), (a, b) => IntValue(a * b))
      case Div(e1, e2) => calculateValue(eval(e1, store, funcStore, classStore), eval(e2, store, funcStore, classStore),
        (a, b) => if (b == 0) UndefinedValue("divide by zero") else IntValue(a / b))
      case Parens(e) => eval(e, store, funcStore, classStore)
      // conditions
      case Neg(c) => {
        val cnd = eval(c, store, funcStore, classStore)
        cnd match {
          case e @ ErrorValue(_) => e
          case v => BoolValue(!v.getBoolValue)
        }
      }
      case Equal(e1, e2) => calculateValue(eval(e1, store, funcStore, classStore), eval(e2, store, funcStore, classStore), (a, b) => BoolValue(a == b))
      case GreaterThan(e1, e2) => calculateValue(eval(e1, store, funcStore, classStore), eval(e2, store, funcStore, classStore), (a, b) => BoolValue(a > b))
      case LessThan(e1, e2) => calculateValue(eval(e1, store, funcStore, classStore), eval(e2, store, funcStore, classStore), (a, b) => BoolValue(a < b))
      case GreaterOE(e1, e2) => calculateValue(eval(e1, store, funcStore, classStore), eval(e2, store, funcStore, classStore), (a, b) => BoolValue(a >= b))
      case LessOE(e1, e2) => calculateValue(eval(e1, store, funcStore, classStore), eval(e2, store, funcStore, classStore), (a, b) => BoolValue(a <= b))
      // functions
      case Call(name, args) => {
        val fdef = funcStore.get(name)
        fdef match {
          case FErr(msg) => UndefinedValue(msg)
          case FDef(fargs, fbody) => {
            val nArgs = fargs.size
            if (nArgs != args.size)
              throw new RuntimeException("Illegal number of function arguments.")
            val vals = args.map(a => eval(a.entry, store, funcStore, classStore))
            val staticScopeStore = new PlainStore()
            for (i <- 0 until nArgs)
              staticScopeStore.put(fargs(i), vals(i))
            execute(fbody, staticScopeStore, funcStore, classStore)
            staticScopeStore.get("res")
          }
        }
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
}