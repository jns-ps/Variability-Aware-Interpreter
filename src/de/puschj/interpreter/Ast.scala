package de.puschj.interpreter

import de.fosd.typechef.conditional._


sealed abstract class ASTNode

sealed abstract class Statement extends ASTNode
case class Assignment(name: String, value: Expression) extends Statement
case class Block(stmts: List[Opt[Statement]]) extends Statement
case class While(cond: Condition, stmt: Statement) extends Statement
case class If(cond: Condition, s1: Statement, s2: Option[Statement]) extends Statement
case class Assert(cond: Condition) extends Statement


sealed abstract class Expression extends ASTNode
case class Num(n: Int) extends Expression
case class Id(x: String) extends Expression
case class Parens(e: Expression) extends Expression
abstract case class BinaryExpression(e1: Expression, e2: Expression) extends Expression
case class Add(override val e1: Expression, override val e2: Expression) extends BinaryExpression(e1, e2)
case class Sub(override val e1: Expression, override val e2: Expression) extends BinaryExpression(e1, e2)
case class Mul(override val e1: Expression, override val e2: Expression) extends BinaryExpression(e1, e2)
case class Div(override val e1: Expression, override val e2: Expression) extends BinaryExpression(e1, e2)



sealed abstract class Condition extends Expression
case class Neg(c: Condition) extends Condition
abstract case class BinaryCondition(e1: Expression, e2: Expression) extends Condition
case class Equal(override val e1: Expression, override val e2: Expression) extends BinaryCondition(e1, e2)
case class GreaterThan(override val e1: Expression, override val e2: Expression) extends BinaryCondition(e1, e2)
case class LessThan(override val e1: Expression, override val e2: Expression) extends BinaryCondition(e1, e2)
case class GreaterOE(override val e1: Expression, override val e2: Expression) extends BinaryCondition(e1, e2)
case class LessOE(override val e1: Expression, override val e2: Expression) extends BinaryCondition(e1, e2)



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

sealed case class ErrorValue(s: String) extends Value {
  def name() = getClass().getCanonicalName();
  
  def getIntValue(): Int = {
    throw new Exception("called getIntValue on "+name()+": "+s)
  }
  def getBoolValue(): Boolean = {
    throw new Exception("called getBoolValue on "+name()+": "+s)
  }
}

case class UndefinedValue(override val s: String) extends ErrorValue(s)
case class NotANumberValue(override val s: String) extends ErrorValue(s)

