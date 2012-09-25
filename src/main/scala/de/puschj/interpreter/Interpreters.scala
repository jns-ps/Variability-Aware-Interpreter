package de.puschj.interpreter

import de.fosd.typechef.conditional._
import de.fosd.typechef.featureexpr.FeatureExprFactory.False
import de.fosd.typechef.featureexpr.FeatureExprFactory.True
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.featureexpr.FeatureExprFactory


object VAInterpreter {
  
  private val LOOP_EXCEEDANCE_VALUE = 1000
  
//  var nWhileLoops = 0
//  var nWhileAbordsOnStart = 0

  @throws(classOf[LoopExceededException])
  def execute(s: Stmt, context: FeatureExpr, store: VAStore, funcStore: VAFuncStore, classStore: VAClassStore): Unit = {
    if (context.isContradiction()) return
    s match {
      case ExprStmt(expr) => eval(One(expr), store, funcStore, classStore)

      case Assign(expr, value) => expr match {
        case Var(name) => store.put(name, Choice(context, eval(value, store, funcStore, classStore), store.get(name)).simplify)
        case Field(e, name) => {
          eval(One(e), store, funcStore, classStore).map(_ match {
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

      case w @ While(c, block) => {
//        nWhileLoops += 1
        var isSat: Boolean = true
        var n = 0
        while (isSat && (n < LOOP_EXCEEDANCE_VALUE)) {
          val x: FeatureExpr = whenTrue(c, store, funcStore, classStore)
          isSat = (context and x).isSatisfiable
          if (isSat)
            execute(block, context and x, store, funcStore, classStore)
//          if ((n == 0) && (!isSat)) nWhileAbordsOnStart += 1
          n += 1
        }
        if (n >= LOOP_EXCEEDANCE_VALUE) {
          throw new LoopExceededException("Exceeded Loop in Statement: \n" + SourceCodePrettyPrinter.prettyPrintNode(w).mkString)
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
        val whentrue: FeatureExpr = whenTrue(One(cnd), store, funcStore, classStore)
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
      case ClassDec(name, args, superClass, optConsts, optFields, optFuncDecs) => {
        val cFuncStore = new VAFuncStore
        for (optFuncDec <- optFuncDecs) {
          val funcDec = optFuncDec.entry
          cFuncStore.put(funcDec.name, Choice(optFuncDec.feature, One(FDef(funcDec.args, funcDec.body)), funcStore.get(funcDec.name)).simplify)
        }
        val cConstStore = new VAStore
        for (optConst <- optConsts) {
          val name = optConst.entry.expr.asInstanceOf[Var].name
          val value = optConst.entry.value
          cConstStore.put(name, Choice(optConst.feature, eval(value, store, funcStore, classStore), cConstStore.get(name)).simplify)
        }
        classStore.put(name, Choice(context, One(VACDef(args, superClass, optFields, cConstStore, cFuncStore)), classStore.get(name)).simplify)
      }
    }
  }

  private def eval(exp: Conditional[Expr], store: VAStore, funcStore: VAFuncStore, classStore: VAClassStore): Conditional[Value] = {
    
    def calculateValue(e1: Expr, e2: Expr, f: (Value, Value) => Value) =
      ConditionalLib.mapCombination(
        eval(One(e1), store, funcStore, classStore),
        eval(One(e2), store, funcStore, classStore),
        (a: Value, b: Value) => propagateError(a, b, (a, b) => f(a, b)))

    def propagateError(a: Value, b: Value, f: (Value, Value) => Value) = {
      (a, b) match {
        case (ErrorValue(s1), ErrorValue(s2)) => ErrorValue("multiple errors") //(s1+";"+s2) 
        case (e: ErrorValue, _) => e
        case (_, e: ErrorValue) => e
        case (a, b) => f(a, b)
      }
    }
    
    exp.mapr(_ match {
      // arithmetic
      case Null => One(NullValue())
      case Num(n) => One(IntValue(n))
      case Var(name) => store.get(name)
      case Add(e1, e2) => calculateValue(e1, e2, (a, b) => IntValue(a.getIntValue + b.getIntValue))
      case Sub(e1, e2) => calculateValue(e1, e2, (a, b) => IntValue(a.getIntValue - b.getIntValue))
      case Mul(e1, e2) => calculateValue(e1, e2, (a, b) => IntValue(a.getIntValue * b.getIntValue))
      case Div(e1, e2) => calculateValue(e1, e2, (a, b) => if (b.getIntValue == 0)
        UndefinedValue("divide by zero")
      else
        IntValue(a.getIntValue / b.getIntValue))
      case Par(e) => eval(One(e), store, funcStore, classStore)

      // conditions
      case Neg(c) => eval(One(c), store, funcStore, classStore).map(_ match {
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
              One(UndefinedValue("illegal arguments size"))
            else {
                val staticScopeStore = new VAStore()
                for (i <- 0 until nArgs)
                    staticScopeStore.put(fargs(i).entry, Choice(args(i).feature and fargs(i).feature, eval(One(args(i).entry), store, funcStore, classStore), One(UndefinedValue("undef func arg"))).simplify)
                execute(fbody, True, staticScopeStore, funcStore, classStore)
                if (!staticScopeStore.contains("res"))
                  One(UndefinedValue("'" + name + "' returning void"))
                else
                  staticScopeStore.get("res")
            }
          }
        })
      }
      case New(name, argsNew) => {
        classStore.get(name).map(_ match {
          case CErr(msg) => UndefinedValue(msg)
          case VACDef(argsClass, superClass, fields, consts, methods) => {

            //            TODO: implement allFields for Superclasses
            //            val allFields = ...

            if (argsClass.size != argsNew.size)
              UndefinedValue("illegal arguments size")
            else {
              val objectStore = new VAStore
              val obj = VAObjectValue(name, objectStore)
              for (i <- 0 until argsClass.size)
                  objectStore.put(argsClass(i).entry, 
                      Choice(argsNew(i).feature and argsClass(i).feature, 
                          eval(One(argsNew(i).entry), store, funcStore, classStore), 
                          One(UndefinedValue("undef constructor arg"))
                      ).simplify)
              objectStore.put("this", One(obj))
              fields.map(f => execute(f.entry, f.feature, objectStore, funcStore, classStore))
              obj
            }
          }
        })
      }
      case Field(expr, name) => {
        eval(One(expr), store, funcStore, classStore).mapr(_ match {
          case e: ErrorValue => One(e)
          case n: NullValue => One(IllegalOPValue("null pointer access: '"+name+"' on "+expr)) //throw new NullPointerException("cannot access field '"+name+"' on null value: "+expr)
          case o: VAObjectValue => {
            // TODO: check feature expression merging here
            classStore.get(o.className).mapr(_ match {
              case CErr(msg) => One(UndefinedValue(msg))
              case VACDef(_, _, _, consts, _) =>
                if (consts.contains(name))
                   consts.get(name)
                else
                   o.getFieldValue(name)
            })
          }
          case x => One(IllegalOPValue("cannot get field of non object Value"))
        })
      }
      case MethodCall(expr, call) => {
        eval(One(expr), store, funcStore, classStore).mapr(_ match {
          case e: ErrorValue => One(e)
          case n: NullValue => One(IllegalOPValue("null pointer access: '"+call.fname+"'"))//throw new NullPointerException("cannot execute method '"+call.fname+"' on null value")
          case v @ VAObjectValue(cName, fields) => {
            classStore.get(cName).mapr(_ match {
              case VACDef(_, _, _, _, classFuncStore) => classFuncStore.get(call.fname).mapr(_ match {
                case FErr(msg) => One(UndefinedValue(msg))
                case FDef(fargs, fbody) => {
                  val nArgs = fargs.size
                  if (nArgs != call.args.size)
                    One(UndefinedValue("illegal arguments size"))
                  else {
                      val vals = call.args.map(a => Choice(a.feature, eval(One(a.entry), store, funcStore, classStore), One(UndefinedValue("undef func arg"))))
                      val staticScopeStore = new VAStore()
                      staticScopeStore.put("this", One(v))
                      for (i <- 0 until nArgs)
                          staticScopeStore.put(fargs(i).entry, 
                              Choice(call.args(i).feature and fargs(i).feature, 
                                  eval(One(call.args(i).entry), store, funcStore, classStore), 
                                  One(UndefinedValue("undef func arg"))
                              ).simplify)
                      execute(fbody, True, staticScopeStore, classFuncStore, classStore)
                      if (!staticScopeStore.contains("res"))
                        One(UndefinedValue("'" + call.fname + "' returning void"))
                      else
                        staticScopeStore.get("res")
                  }
                }
              })
              case e: CErr => One(UndefinedValue("class '" + cName + "' not defined"))
            })
          }
          case x => One(IllegalOPValue("cannot invoke method on: '" + x.getClass.getCanonicalName + "'"))
        })
      }
    })
  }

  private def whenTrue(e: Conditional[Expr], store: VAStore, funcStore: VAFuncStore, classStore: VAClassStore): FeatureExpr =
    eval(e, store, funcStore, classStore).when(_ match {
      case ErrorValue(_) => false
      case value => value.getBoolValue
    })

//  def allFields(className: String, classStore: VAClassStore) : Conditional[List[String]] = {
//    if (className.equals("Object")) 
//      One(List.empty[String])
//    else {
//      classStore.get(className).mapr(_ match {
//        case VACDef(args, superClass, fields, consts, methods) => {
//          var res: Conditional[List[String]] = One(List.empty[String])
//          val assignmentVars = fields.map(a => Opt(a.feature, a.entry.expr.asInstanceOf[Id].x))
//          for (variable <- (args ++ assignmentVars)) {
//              res = res.mapr(l => Choice(variable.feature, One(variable.entry :: l), One(l))).simplify
//          }
//          res.simplify
//        }
//        case CErr(_) => One(List.empty[String])
//      })
//    }
//  }

  // TODO: lookupMethod() for Superclasses
}

object PlainInterpreter {
  
  private val LOOP_EXCEEDANCE_VALUE = 1000

  @throws(classOf[LoopExceededException])
  def execute(s: Stmt, store: VAStore, funcStore: VAFuncStore, classStore: VAClassStore): Unit = {
    s match {
      case ExprStmt(expr) => eval(expr, store, funcStore, classStore)

      case Assign(expr, One(value)) => expr match {
        case Var(name) => store.put(name, One(eval(value, store, funcStore, classStore)))
        case Field(e, name) => {
          eval(e, store, funcStore, classStore) match {
            case e: ErrorValue => e
            case VAObjectValue(_, fields) => {
              // TODO: consider check for field existance
              // TODO: consider blocking assignments to variables named the same as existing constants
              fields.put(name, One(eval(value, store, funcStore, classStore)))
            }
            case v => IllegalOPValue("field access to non object value")
          }
        }
        case e => IllegalOPValue("assignment to non variable")
      }

      case Block(stmts) => for (stm <- stmts) execute(stm.entry, store, funcStore, classStore)
      case w @ While(One(c), s) => {
        var n = 0
        var cnd = true
        while (cnd && (n < LOOP_EXCEEDANCE_VALUE)) {
          cnd = eval(c, store, funcStore, classStore) match {
            case ErrorValue(_) => false
            case v => v.getBoolValue
          }
          if (cnd)
            execute(s, store, funcStore, classStore)
          n += 1
        }
        if (n >= LOOP_EXCEEDANCE_VALUE) {
          throw new LoopExceededException("Exceeded Loop in Statement: " + w)
        }
      }
      case If(One(c), s1, s2) => {
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
        // TODO: remove assertion
        args.map(f => assert(f.feature == True))
        funcStore.put(name, One(FDef(args, body)))
      }
      case ClassDec(name, args, superClass, optConsts, optFields, optFuncDecs) => {
        val cFuncStore = new VAFuncStore
        for (optFuncDec <- optFuncDecs) {
          val funcDec = optFuncDec.entry
          // TODO: remove assertion
          funcDec.args.map(f => assert(f.feature == True))
          cFuncStore.put(funcDec.name, One(FDef(funcDec.args, funcDec.body)))
        }
        val cConstStore = new VAStore
        for (optConst <- optConsts) {
          assert(optConst.feature == True)
          val name = optConst.entry.expr.asInstanceOf[Var].name
          val value = optConst.entry.value
          cConstStore.put(name, One(eval(value.asInstanceOf[One[Expr]].value, store, funcStore, classStore)))
        }
        classStore.put(name, One(VACDef(args, superClass, optFields, cConstStore, cFuncStore)))
      }
    }
  }

  private def eval(exp: Expr, store: VAStore, funcStore: VAFuncStore, classStore: VAClassStore): Value = {

    def calculateValue(e1: Expr, e2: Expr, f: (Value, Value) => Value): Value =
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
      case Null => NullValue()
      case Num(n) => IntValue(n)
      case Var(name) => store.get(name) match {
        case One(value) => value
        case _ => throw new AssertionError("conditional Id '"+name+"' in plain interpreter")
      }
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
      case And(e1, e2) => calculateValue(e1, e2, (a, b) => BoolValue(a.getBoolValue && b.getBoolValue))
      case Or(e1, e2) => calculateValue(e1, e2, (a, b) => BoolValue(a.getBoolValue || b.getBoolValue))

      // functions
      case Call(name, args) => {
        funcStore.get(name) match {
            case One(fdef) => fdef match {
              case FErr(msg) => UndefinedValue(msg)
              case FDef(fargs, fbody) => {
                val nArgs = fargs.size
                if (nArgs != args.size)
                  throw new RuntimeException("Illegal number of function arguments.")
                val vals = args.map(a => eval(a.entry, store, funcStore, classStore))
                val staticScopeStore = new VAStore()
                for (i <- 0 until nArgs) {
                  // TODO: remove assertion
                  assert(fargs(i).feature == True)
                  staticScopeStore.put(fargs(i).entry, One(vals(i)))
                }
                  
                execute(fbody, staticScopeStore, funcStore, classStore)
                if (!staticScopeStore.contains("res"))
                  UndefinedValue("'" + name + "' returning void")
                else
                  staticScopeStore.get("res") match {
                    case One(res) => res
                    case _ => throw new AssertionError("conditional function def '"+name+"' called in plain interpreter")
                  }
              }
            }
            case _ => throw new AssertionError("conditional function def '"+name+"' called in plain interpreter")
        }
      }
      case New(name, argsNew) => {
        classStore.get(name) match {
            case One(cdef) => cdef match {
              case CErr(msg) => UndefinedValue(msg)
              case VACDef(argsClass, superClass, fields, consts, methods) => {
    
                //            TODO: implement allFields for Superclasses
                //            val allFields = ...
    
                if (argsClass.size != argsNew.size)
                  throw new RuntimeException("Illegal number of construction arguments: "+name+" ("+argsNew.size+")")
                val vals = argsNew.map(a => eval(a.entry, store, funcStore, classStore))
                val objectStore = new VAStore
                val obj = VAObjectValue(name, objectStore)
                for (i <- 0 until argsClass.size) {
                  // TODO: remove assertion
                  assert(argsClass(i).feature == True)
                  objectStore.put(argsClass(i).entry, One(vals(i)))
                }
                objectStore.put("this", One(obj))
                fields.map(f => {
                  // TODO: remove assertion
                  assert(f.feature == True)
                  execute(f.entry, objectStore, funcStore, classStore)
                })
                obj
              }
            }
            case _ => throw new AssertionError("conditional class definition '"+name+"' on 'new' in plain interpreter")
        }
      }
      case Field(expr, name) => {
        eval(expr.asInstanceOf[One[Expr]].value, store, funcStore, classStore) match {
          case e: ErrorValue => e
          case o: VAObjectValue => {
            classStore.get(o.className) match {
              case One(cdef) => cdef match {
                  case CErr(msg) => UndefinedValue(msg)
                  case VACDef(_, _, _, consts, _) =>
                    if (consts.contains(name)) {
                      consts.get(name) match {
                        case One(const) => const
                        case _ => throw new AssertionError("const '"+name+"' conditional in plain interpreter")
                      }
                    }
                    else
                      o.getFieldValue(name) match {
                        case One(value) => value
                        case _ => throw new AssertionError("conditional field value '"+name+"' accessed in plain interpreter")
                      }
              }
              case _ => throw new AssertionError("conditional class definition '"+o.className+"' accessed in plain interpreter")
            }
          }
          case x => IllegalOPValue("cannot get field of non object Value")
        }
      }
      case MethodCall(expr, call) => {
        eval(expr.asInstanceOf[One[Expr]].value, store, funcStore, classStore) match {
          case e: ErrorValue => e
          case v @ VAObjectValue(cName, fields) => {
            classStore.get(cName) match {
                case One(cdef) => cdef match {
                  case VACDef(_, _, _, _, funcStore) => funcStore.get(call.fname) match {
                    case One(fdef) => fdef match {
                      case FErr(msg) => UndefinedValue(msg)
                      case FDef(fargs, fbody) => {
                        val nArgs = fargs.size
                        if (nArgs != call.args.size)
                          throw new RuntimeException("Illegal number of arguments: '"+v.className+"."+call.fname+"'")
                        val vals = call.args.map(a => eval(a.entry, store, funcStore, classStore))
                        val staticScopeStore = new VAStore()
                        staticScopeStore.put("this", One(v))
                        for (i <- 0 until nArgs) {
                          // TODO: remove assertion
                          assert(fargs(i).feature == True)
                          staticScopeStore.put(fargs(i).entry, One(vals(i)))
                        }
                              
                        execute(fbody, staticScopeStore, funcStore, classStore)
                        if (!staticScopeStore.contains("res"))
                          UndefinedValue("'" + call.fname + "' returning void")
                        else
                          staticScopeStore.get("res") match {
                            case One(res) => res
                            case _ => throw new AssertionError("conditional result for '"+call.fname+"' in plain interpreter")
                          }
                      }
                    }
                    case _ => throw new AssertionError("function '"+call.fname+"' conditional in plain interpreter")
                  }
                  case e: CErr => UndefinedValue("class '" + cName + "' not defined")
                }
                case _ => throw new AssertionError("conditional class definition for '"+cName+"' in plain interpreter")
            }
          }
          case x => IllegalOPValue("cannot invoke method on: '" + x.getClass.getCanonicalName + "'")
        }
      }
    }
  }
}