package de.puschj.interpreter

import de.fosd.typechef.conditional._
import de.fosd.typechef.featureexpr.FeatureExprFactory.False
import de.fosd.typechef.featureexpr.FeatureExprFactory.True
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.featureexpr.FeatureExprFactory

object VAInterpreter {

  @throws(classOf[LoopExceededException])
  def execute(s: Statement, context: FeatureExpr, store: VAStore, funcStore: VAFuncStore, classStore: VAClassStore): Unit = {
    if (context.isContradiction()) return
    s match {
      case ExpressionStmt(expr) => eval(expr, store, funcStore, classStore)

      case Assignment(expr, value) => expr match {
        case Id(name) => store.put(name, Choice(context, eval(value, store, funcStore, classStore), store.get(name)).simplify)
        case Field(e, name) => {
          eval(e, store, funcStore, classStore).map(_ match {
            case e: ErrorValue => e
            case VAObjectValue(cName, fields) => {
              // TODO: consider check for field existance
              // TODO: consider blocking assignments to variables named the same as existing constants
              fields.put(name, Choice(context, eval(value, store, funcStore, classStore), fields.get(name)).simplify)
            }
            case v => IllegalOPValue("field access to non object value")
          })
        }
        case e => IllegalOPValue("assignment to non variable")
      }
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
      case ClassDec(name, superClass, fields, consts, optFuncDecs) => {
        val cFuncStore = new VAFuncStore
        for (optFuncDec <- optFuncDecs) {
          val funcDec = optFuncDec.entry
          cFuncStore.put(funcDec.name, Choice(optFuncDec.feature, One(FDef(funcDec.args, funcDec.body)), funcStore.get(funcDec.name)).simplify)
        }
        val evaluatedConsts = consts.map(c => (c._1, eval(c._2, store, funcStore, classStore)))
        classStore.put(name, Choice(context, One(VACDef(superClass, fields, evaluatedConsts, cFuncStore)), classStore.get(name)).simplify)
      }
    }
  }

  private def eval(exp: Expression, store: VAStore, funcStore: VAFuncStore, classStore: VAClassStore): Conditional[Value] = {

    def calculateValue(e1: Expression, e2: Expression, f: (Value, Value) => Value) =
      ConditionalLib.mapCombination(
        eval(e1, store, funcStore, classStore),
        eval(e2, store, funcStore, classStore),
        (a: Value, b: Value) => propagateError(a, b, (a, b) => f(a, b)))

    def propagateError(a: Value, b: Value, f: (Value, Value) => Value) = {
      (a, b) match {
        case (ErrorValue(s1), ErrorValue(s2)) => ErrorValue("multiple errors") //(s1+";"+s2) 
        case (e: ErrorValue, _) => e
        case (_, e: ErrorValue) => e
        case (a, b) => f(a, b)
      }
    }

    exp match {
      // arithmetic
      case Num(n) => One(IntValue(n))
      case Id(x) => store.get(x)
      case Add(e1, e2) => calculateValue(e1, e2, (a, b) => IntValue(a.getIntValue + b.getIntValue))
      case Sub(e1, e2) => calculateValue(e1, e2, (a, b) => IntValue(a.getIntValue - b.getIntValue))
      case Mul(e1, e2) => calculateValue(e1, e2, (a, b) => IntValue(a.getIntValue * b.getIntValue))
      case Div(e1, e2) => calculateValue(e1, e2, (a, b) => if (b.getIntValue == 0)
        UndefinedValue("divide by zero")
      else
        IntValue(a.getIntValue / b.getIntValue))
      case Par(e) => eval(e, store, funcStore, classStore)

      // conditions
      case Neg(c) => eval(c, store, funcStore, classStore).map(_ match {
        case e @ ErrorValue(_) => e
        case v => BoolValue(!v.getBoolValue())
      })
      case Bool(b) => One(BoolValue(b))
      case Eq(e1, e2) => calculateValue(e1, e2, (a, b) => BoolValue(a equals b))
      case NEq(e1, e2) => calculateValue(e1, e2, (a, b) => BoolValue(!(a equals b)))
      case GrT(e1, e2) => calculateValue(e1, e2, (a, b) => BoolValue(a.getIntValue > b.getIntValue))
      case LeT(e1, e2) => calculateValue(e1, e2, (a, b) => BoolValue(a.getIntValue < b.getIntValue))
      case GoE(e1, e2) => calculateValue(e1, e2, (a, b) => BoolValue(a.getIntValue >= b.getIntValue))
      case LoE(e1, e2) => calculateValue(e1, e2, (a, b) => BoolValue(a.getIntValue <= b.getIntValue))
      case And(e1, e2) => calculateValue(e1, e2, (a, b) => BoolValue(a.getBoolValue && b.getBoolValue))
      case Or(e1, e2) => calculateValue(e1, e2, (a, b) => BoolValue(a.getBoolValue || b.getBoolValue))

      // functions
      case Call(name, args) => {
        // TODO: check correctness of all mapr ... !!!
        funcStore.get(name).mapr(_ match {
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
            if (!staticScopeStore.contains("res"))
              One(UndefinedValue("'" + name + "' returning void"))
            else
              staticScopeStore.get("res")
          }
        })
      }
      case New(name, args) => {
        classStore.get(name).map(_ match {
          case CErr(msg) => UndefinedValue(msg)
          case VACDef(superClass, fields, consts, methods) => {

            //            TODO: implement allFields for Superclasses
            //            val allFields = ...

            if (fields.size != args.size)
              throw new RuntimeException("Illegal number of construction arguments.")
            val vals = args.map(a => Choice(a.feature, eval(a.entry, store, funcStore, classStore), One(UndefinedValue("undef constructor arg"))))
            val objectStore = new VAStore
            for (i <- 0 until fields.size)
              objectStore.put(fields(i), vals(i))
            VAObjectValue(name, objectStore)
          }
        })
      }
      case Field(expr, name) => {
        eval(expr, store, funcStore, classStore).mapr(_ match {
          case e: ErrorValue => One(e)
          case o: VAObjectValue => {
            // TODO: check feature expression merging here
            classStore.get(o.className).mapr(_ match {
              case CErr(msg) => One(UndefinedValue(msg))
              case VACDef(superClass, fields, consts, methods) =>
                consts.getOrElse(name, o.getFieldValue(name))
            })
          }
          case x => One(IllegalOPValue("cannot get field of non object Value"))
        })
      }
      case MethodCall(expr, call) => {
        eval(expr, store, funcStore, classStore).mapr(_ match {
          case e: ErrorValue => One(e)
          case v @ VAObjectValue(cName, fields) => {
            classStore.get(cName).mapr(_ match {
              case VACDef(_, _, _, classFuncStore) => classFuncStore.get(call.fname).mapr(_ match {
                case FErr(msg) => One(UndefinedValue(msg))
                case FDef(fargs, fbody) => {
                  val nArgs = fargs.size
                  if (nArgs != call.args.size)
                    throw new RuntimeException("Illegal number of arguments.")
                  val vals = call.args.map(a => Choice(a.feature, eval(a.entry, store, funcStore, classStore), One(UndefinedValue("undef func arg"))))
                  val staticScopeStore = new VAStore()
                  staticScopeStore.put("this", One(v))
                  for (i <- 0 until nArgs)
                    staticScopeStore.put(fargs(i), vals(i))
                  execute(fbody, True, staticScopeStore, classFuncStore, classStore)
                  if (!staticScopeStore.contains("res"))
                    One(UndefinedValue("'" + call.fname + "' returning void"))
                  else
                    staticScopeStore.get("res")
                }
              })
              case e: CErr => One(UndefinedValue("class '" + cName + "' not defined"))
            })
          }
          case x => One(IllegalOPValue("cannot invoke method on: '" + x.getClass.getCanonicalName + "'"))
        })
      }
    }
  }

  private def whenTrue(e: Expression, store: VAStore, funcStore: VAFuncStore, classStore: VAClassStore): FeatureExpr =
    eval(e, store, funcStore, classStore).when(_ match {
      case ErrorValue(_) => false
      case value => value.getBoolValue
    })

  //  private def allFields(className: String, classStore: VAClassStore) : List[Conditional[String]] = {
  //    if (className.equals("Object")) List.empty[Conditional[String]]
  //    else {
  //      classStore.get(className).map(_ match {
  //        TODO implement
  //      })
  //    }
  //  }

  // TODO: lookupMethod() for Superclasses
}

object PlainInterpreter {

  @throws(classOf[LoopExceededException])
  def execute(s: Statement, store: PlainStore, funcStore: PlainFuncStore, classStore: PlainClassStore): Unit = {
    s match {
      case ExpressionStmt(expr) => eval(expr, store, funcStore, classStore)

      case Assignment(expr, value) => expr match {
        case Id(name) => store.put(name, eval(value, store, funcStore, classStore))
        case Field(e, name) => {
          eval(e, store, funcStore, classStore) match {
            case e: ErrorValue => e
            case PlainObjectValue(cName, fields) => {
              // TODO: consider check for field existance
              // TODO: consider blocking assignments to variables named the same as existing constants
              fields.put(name, eval(value, store, funcStore, classStore))
            }
            case v => IllegalOPValue("field access to non object value")
          }
        }
        case e => IllegalOPValue("assignment to non variable")
      }

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
      case ClassDec(name, superClass, fields, consts, optFuncDecs) => {
        val cFuncStore = new PlainFuncStore
        for (optFuncDec <- optFuncDecs) {
          val funcDec = optFuncDec.entry
          cFuncStore.put(funcDec.name, FDef(funcDec.args, funcDec.body))
        }
        val evaluatedConsts = consts.map(c => (c._1, eval(c._2, store, funcStore, classStore)))
        classStore.put(name, PlainCDef(superClass, fields, evaluatedConsts, cFuncStore))
      }
    }
  }

  private def eval(exp: Expression, store: PlainStore, funcStore: PlainFuncStore, classStore: PlainClassStore): Value = {

    def calculateValue(e1: Expression, e2: Expression, f: (Value, Value) => Value): Value =
      propagateError(eval(e1, store, funcStore, classStore), eval(e2, store, funcStore, classStore), f)

    def propagateError(a: Value, b: Value, f: (Value, Value) => Value) = {
      (a, b) match {
        case (ErrorValue(s1), ErrorValue(s2)) => ErrorValue("multiple errors") //(s1+";"+s2) 
        case (e: ErrorValue, _) => e
        case (_, e: ErrorValue) => e
        case (a, b) => f(a, b)
      }
    }

    exp match {
      // arithmetic
      case Num(n) => IntValue(n)
      case Id(x) => store.get(x)
      case Add(e1, e2) => calculateValue(e1, e2, (a, b) => IntValue(a.getIntValue + b.getIntValue))
      case Sub(e1, e2) => calculateValue(e1, e2, (a, b) => IntValue(a.getIntValue - b.getIntValue))
      case Mul(e1, e2) => calculateValue(e1, e2, (a, b) => IntValue(a.getIntValue * b.getIntValue))
      case Div(e1, e2) => calculateValue(e1, e2, (a, b) => if (b.getIntValue == 0)
        UndefinedValue("divide by zero")
      else
        IntValue(a.getIntValue / b.getIntValue))
      case Par(e) => eval(e, store, funcStore, classStore)

      // conditions
      case Neg(c) => eval(c, store, funcStore, classStore) match {
        case e @ ErrorValue(_) => e
        case v => BoolValue(!v.getBoolValue)
      }
      case Bool(b) => BoolValue(b)
      case Eq(e1, e2) => calculateValue(e1, e2, (a, b) => BoolValue(a equals b))
      case NEq(e1, e2) => calculateValue(e1, e2, (a, b) => BoolValue(!(a equals b)))
      case GrT(e1, e2) => calculateValue(e1, e2, (a, b) => BoolValue(a.getIntValue > b.getIntValue))
      case LeT(e1, e2) => calculateValue(e1, e2, (a, b) => BoolValue(a.getIntValue < b.getIntValue))
      case GoE(e1, e2) => calculateValue(e1, e2, (a, b) => BoolValue(a.getIntValue >= b.getIntValue))
      case LoE(e1, e2) => calculateValue(e1, e2, (a, b) => BoolValue(a.getIntValue <= b.getIntValue))

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
            if (!staticScopeStore.contains("res"))
              UndefinedValue("'" + name + "' returning void")
            else
              staticScopeStore.get("res")
          }
        }
      }
      case New(name, args) => {
        classStore.get(name) match {
          case CErr(msg) => UndefinedValue(msg)
          case PlainCDef(superClass, fields, consts, methods) => {

            //            TODO: implement allFields for Superclasses
            //            val allFields = ...

            if (fields.size != args.size)
              throw new RuntimeException("Illegal number of construction arguments.")
            val vals = args.map(a => eval(a.entry, store, funcStore, classStore))
            val objectStore = new PlainStore
            for (i <- 0 until fields.size)
              objectStore.put(fields(i), vals(i))
            PlainObjectValue(name, objectStore)
          }
        }
      }
      case Field(expr, name) => {
        eval(expr, store, funcStore, classStore) match {
          case e: ErrorValue => e
          case o: PlainObjectValue => {
            classStore.get(o.className) match {
              case CErr(msg) => UndefinedValue(msg)
              case PlainCDef(superClass, fields, consts, methods) =>
                consts.getOrElse(name, o.getFieldValue(name))
            }
          }
          case x => IllegalOPValue("cannot get field of non object Value")
        }
      }
      case MethodCall(expr, call) => {
        eval(expr, store, funcStore, classStore) match {
          case e: ErrorValue => e
          case v @ PlainObjectValue(cName, fields) => {
            classStore.get(cName) match {
              case PlainCDef(_, _, _, funcStore) => funcStore.get(call.fname) match {
                case FErr(msg) => UndefinedValue(msg)
                case FDef(fargs, fbody) => {
                  val nArgs = fargs.size
                  if (nArgs != call.args.size)
                    throw new RuntimeException("Illegal number of arguments.")
                  val vals = call.args.map(a => eval(a.entry, store, funcStore, classStore))
                  val staticScopeStore = new PlainStore()
                  staticScopeStore.put("this", v)
                  for (i <- 0 until nArgs)
                    staticScopeStore.put(fargs(i), vals(i))
                  execute(fbody, staticScopeStore, funcStore, classStore)
                  if (!staticScopeStore.contains("res"))
                    UndefinedValue("'" + call.fname + "' returning void")
                  else
                    staticScopeStore.get("res")
                }
              }
              case e: CErr => UndefinedValue("class '" + cName + "' not defined")
            }
          }
          case x => IllegalOPValue("cannot invoke method on: '" + x.getClass.getCanonicalName + "'")
        }
      }
    }
  }
}