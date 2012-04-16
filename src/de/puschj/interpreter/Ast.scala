package de.puschj.interpreter


abstract class ASTNode {
  def accept(visitor: Visitor) = visitor.visit(this)
}

trait Visitor {
  def visit(node: ASTNode)
}


class Program(stmts: List[Statement]) {
  def run(env: Environment) = stmts foreach (StatementExecutor.execute(_, env))
  def print() = println(stmts)
}

sealed abstract class Statement extends ASTNode
case class Assignment(name: String, value: Expression) extends Statement
case class Block(stmts: List[Statement]) extends Statement
case class While(cond: Condition, stmt: Statement) extends Statement
case class If(cond: Condition, s1: Statement, s2: Option[Statement]) extends Statement

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


sealed abstract class Expression
case class Num(n: Int) extends Expression
case class Add(e1: Expression, e2: Expression) extends Expression
case class Sub(e1: Expression, e2: Expression) extends Expression
case class Mul(e1: Expression, e2: Expression) extends Expression
case class Div(e1: Expression, e2: Expression) extends Expression
case class Id(x: String) extends Expression
case class Parens(e: Expression) extends Expression

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


sealed abstract class Condition
case class GreaterThan(e1: Expression, e2: Expression) extends Condition
case class LessThan(e1: Expression, e2: Expression) extends Condition
case class GreaterOrEqualThan(e1: Expression, e2: Expression) extends Condition
case class LessOrEqualThan(e1: Expression, e2: Expression) extends Condition
case class Neg(c: Condition) extends Condition

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