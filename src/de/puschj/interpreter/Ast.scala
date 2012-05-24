package de.puschj.interpreter

import de.fosd.typechef.conditional._


trait Visitor {
  def visit(node: ASTNode)
}

abstract class ASTNode {
  def accept(visitor: Visitor) = visitor.visit(this)
}

case class Program(stmts: List[Opt[Statement]]) {
  def run(store: Store): Store = {
    for(stm <- stmts) Interpreter.execute(stm.entry, stm.feature, store)
    return store
  }
  def print() = println(stmts)
}

sealed abstract class Statement extends ASTNode
case class Assignment(name: String, value: Expression) extends Statement
case class Block(stmts: List[Opt[Statement]]) extends Statement
case class While(cond: Condition, stmt: Statement) extends Statement
case class If(cond: Condition, s1: Statement, s2: Option[Statement]) extends Statement
case class Assert(cond: Condition) extends Statement


sealed abstract class Expression extends ASTNode
case class Num(n: Int) extends Expression
case class Add(e1: Expression, e2: Expression) extends Expression
case class Sub(e1: Expression, e2: Expression) extends Expression
case class Mul(e1: Expression, e2: Expression) extends Expression
case class Div(e1: Expression, e2: Expression) extends Expression
case class Id(x: String) extends Expression
case class Parens(e: Expression) extends Expression


sealed abstract class Condition extends Expression
case class Equal(e1: Expression, e2: Expression) extends Condition
case class GreaterThan(e1: Expression, e2: Expression) extends Condition
case class LessThan(e1: Expression, e2: Expression) extends Condition
case class GreaterOE(e1: Expression, e2: Expression) extends Condition
case class LessOE(e1: Expression, e2: Expression) extends Condition
case class Neg(c: Condition) extends Condition


sealed trait Value {
  def getIntValue(): Int
  def getBoolValue(): Boolean
}
// TODO: create seperate exceptions

case class IntValue(i: Int) extends Value {
  def getIntValue(): Int = {
    return i
  }
  
  def getBoolValue(): Boolean = {
    throw new Exception("called getBoolValue on IntValue")
  }
}

case class BoolValue(b: Boolean) extends Value {
  def getIntValue(): Int = {
    throw new Exception("called getIntValue on BoolValue")
  }
  
  def getBoolValue(): Boolean = {
    return b
  }
}

case class UndefinedValue(s: String) extends Value {
  def getIntValue(): Int = {
    throw new Exception("called getIntValue on UndefinedValue: "+s)
  }
  def getBoolValue(): Boolean = {
    throw new Exception("called getBoolValue on UndefinedValue: "+s)
  }
}

