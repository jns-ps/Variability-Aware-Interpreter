package de.fosd.vainterpretation.interpreter

import de.fosd.typechef.conditional._
import de.fosd.typechef.featureexpr.FeatureExprFactory.False
import de.fosd.typechef.featureexpr.FeatureExprFactory.True
import de.fosd.typechef.featureexpr.FeatureExpr
import de.fosd.typechef.featureexpr.FeatureExprFactory
import InterpreterConstraints._
import SourceCodePrettyPrinter.print
import SourceCodePrettyPrinter.printNode

object InterpreterConstraints {
  val LOOP_EXCEEDANCE_VALUE = 10000000
}

object VAInterpreter {

  @throws(classOf[LoopExceededException])
  def execute(s: Stmt, vctx: FeatureExpr, store: VAStore, funcStore: VAFuncStore): Unit = {
    if (vctx.isContradiction()) return
    s match {
      case Assign(name, value) => store.put(name, Choice(vctx, eval(value, store, funcStore), store.get(name)).simplify)

      case Block(stmts) => {
        for (Opt(fe, stm) <- stmts) 
          execute(stm, fe and vctx, store, funcStore)
      }

      case w@While(c, block) => {
        var isSat: Boolean = true
        var n = 0
        
        while (isSat && (n < LOOP_EXCEEDANCE_VALUE)) {
          val truectx: FeatureExpr = whenTrue(c, store, funcStore)
          val iterctx = vctx and truectx
          isSat = iterctx.isSatisfiable
          if (isSat)
            execute(block, iterctx, store, funcStore)
          n += 1
        }
        if (n >= LOOP_EXCEEDANCE_VALUE) {
          throw new LoopExceededException("Exceeded Loop in Statement: \n" + printNode(w))
        }
      }
      case If(c, s1, s2) => {
        val x: FeatureExpr = whenTrue(c, store, funcStore)
        if (vctx.isSatisfiable) {
          if (x.isSatisfiable)
            execute(s1, vctx and x, store, funcStore)
          if (s2.isDefined)
            execute(s2.get, vctx andNot x, store, funcStore)
        }
      }
      case Assert(cnd) => {
        val whentrue: FeatureExpr = whenTrue(One(cnd), store, funcStore)
        val equivToContext: Boolean = whentrue.equivalentTo(vctx)
        if (!(whentrue.isTautology || equivToContext)) {
          throw new AssertionError("violation of " + 
            printNode(cnd) +
            "\nexpected to be 'true' in context: " + vctx +
            "\nactually was 'true' in context: " + whentrue)
        }
      }
      case FuncDec(name, args, body) => {
        funcStore.put(name, Choice(vctx, One(FDef(args, body)), funcStore.get(name)).simplify)
      }
    }
  }

  private def eval(exp: Conditional[Expr], store: VAStore, funcStore: VAFuncStore): Conditional[Value] = {
    
    def calculateValue(e1: Expr, e2: Expr, f: (Value, Value) => Value) =
      ConditionalLib.mapCombination(
        eval(One(e1), store, funcStore),
        eval(One(e2), store, funcStore),
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
      case Par(e) => eval(One(e), store, funcStore)
      case Add(e1, e2) => calculateValue(e1, e2, (a, b) => IntValue(a.getIntValue + b.getIntValue))
      case Sub(e1, e2) => calculateValue(e1, e2, (a, b) => IntValue(a.getIntValue - b.getIntValue))
      case Mul(e1, e2) => calculateValue(e1, e2, (a, b) => IntValue(a.getIntValue * b.getIntValue))
      case Div(e1, e2) => calculateValue(e1, e2, (a, b) =>
          if (b.getIntValue == 0)
            UndefinedValue("divide by zero")
          else
            IntValue(a.getIntValue / b.getIntValue)
      )

      // conditions
      case Neg(c) => eval(One(c), store, funcStore).map(_ match {
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
        funcStore.get(name).mapr(_ match {
          case FErr(msg) => One(UndefinedValue(msg))
          case FDef(fargs, fbody) => {
            val nArgs = fargs.size
            if (nArgs != args.size)
              One(UndefinedValue("illegal arguments size"))
            else {
                val staticScopeStore = new VAStore()
                for (i <- 0 until nArgs)
                    staticScopeStore.put(fargs(i).entry, Choice(args(i).feature and fargs(i).feature, eval(One(args(i).entry), store, funcStore), One(UndefinedValue("undef func arg"))).simplify)
                execute(fbody, True, staticScopeStore, funcStore)
                if (!staticScopeStore.contains("res"))
                  One(UndefinedValue("'" + name + "' returning void"))
                else
                  staticScopeStore.get("res")
            }
          }
        })
      }
    })
  }

  private def whenTrue(e: Conditional[Expr], store: VAStore, funcStore: VAFuncStore): FeatureExpr =
    eval(e, store, funcStore).when(_ match {
      case ErrorValue(_) => false
      case value => value.getBoolValue
    })
}

object PlainInterpreter {
  
  @throws(classOf[LoopExceededException])
  def execute(s: Stmt, store: PlainStore, funcStore: PlainFuncStore): Unit = {
    s match {
      case Assign(name, One(value)) => store.put(name, eval(value, store, funcStore))

      case Block(stmts) => for (stm <- stmts) execute(stm.entry, store, funcStore)
      case w@While(One(c), s) => {
        var n = 0
        var cnd = true
        while (cnd && (n < LOOP_EXCEEDANCE_VALUE)) {
          cnd = eval(c, store, funcStore) match {
            case ErrorValue(_) => false
            case v => v.getBoolValue
          }
          if (cnd)
            execute(s, store, funcStore)
          n += 1
        }
        if (n >= LOOP_EXCEEDANCE_VALUE) {
          throw new LoopExceededException("Exceeded Loop in Statement: " + w)
        }
      }
      case If(One(c), s1, s2) => {
        val cnd = eval(c, store, funcStore)
        if (!cnd.isInstanceOf[ErrorValue] && cnd.getBoolValue)
          execute(s1, store, funcStore)
        else if (s2.isDefined)
          execute(s2.get, store, funcStore)

      }
      case Assert(c) => {
        val cnd = eval(c, store, funcStore)
        if (cnd.isInstanceOf[ErrorValue] || !cnd.getBoolValue)
          throw new AssertionError("violation of " + cnd)
      }
      case FuncDec(name, args, body) => {
        // TODO: remove assertion
        args.map(f => assert(f.feature == True))
        funcStore.put(name, FDef(args, body))
      }
    }
  }

  private def eval(exp: Expr, store: PlainStore, funcStore: PlainFuncStore): Value = {

    def calculateValue(e1: Expr, e2: Expr, f: (Value, Value) => Value): Value =
      propagateError(eval(e1, store, funcStore), eval(e2, store, funcStore), f)

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
      case Var(name) => store.get(name)
      case Add(e1, e2) => calculateValue(e1, e2, (a, b) => IntValue(a.getIntValue + b.getIntValue))
      case Sub(e1, e2) => calculateValue(e1, e2, (a, b) => IntValue(a.getIntValue - b.getIntValue))
      case Mul(e1, e2) => calculateValue(e1, e2, (a, b) => IntValue(a.getIntValue * b.getIntValue))
      case Div(e1, e2) => calculateValue(e1, e2, (a, b) => if (b.getIntValue == 0)
        UndefinedValue("divide by zero")
      else
        IntValue(a.getIntValue / b.getIntValue))
      case Par(e) => eval(e, store, funcStore)

      // conditions
      case Neg(c) => eval(c, store, funcStore) match {
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
          case FErr(msg) => UndefinedValue(msg)
          case FDef(fargs, fbody) => {
            val nArgs = fargs.size
            if (nArgs != args.size)
              throw new RuntimeException("Illegal number of function arguments.")
            val vals = args.map(a => eval(a.entry, store, funcStore))
            val staticScopeStore = new PlainStore()
            for (i <- 0 until nArgs) {
              // TODO: remove assertion
              assert(fargs(i).feature == True)
              staticScopeStore.put(fargs(i).entry, vals(i))
            }
              
            execute(fbody, staticScopeStore, funcStore)
            if (!staticScopeStore.contains("res"))
              UndefinedValue("'" + name + "' returning void")
            else
              staticScopeStore.get("res")
          }
        }
      }
    }
  }
}