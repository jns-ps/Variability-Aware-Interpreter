package de.puschj.interpreter

import de.fosd.typechef.conditional._


sealed abstract class ASTNode

// =====================
// Expressions
// =====================

sealed abstract class Expression extends ASTNode
case class Num(n: Int) extends Expression
case class Id(x: String) extends Expression
case class Par(e: Expression) extends Expression
abstract case class BinaryExpression(e1: Expression, e2: Expression) extends Expression
case class Add(override val e1: Expression, override val e2: Expression) extends BinaryExpression(e1, e2)
case class Sub(override val e1: Expression, override val e2: Expression) extends BinaryExpression(e1, e2)
case class Mul(override val e1: Expression, override val e2: Expression) extends BinaryExpression(e1, e2)
case class Div(override val e1: Expression, override val e2: Expression) extends BinaryExpression(e1, e2)

// functions
case class Call(fname: String, args: List[Opt[Expression]]) extends Expression

// classes
case class New(name: String, args: List[Opt[Expression]]) extends Expression
case class Field(expr: Expression, name: String) extends Expression
case class MethodCall(expr: Expression, call: Call) extends Expression

// =====================
// Conditions
// =====================

sealed abstract class Condition extends Expression
case class Bool(b: Boolean) extends Condition
case class Neg(c: Condition) extends Condition
abstract case class BinaryCondition(e1: Expression, e2: Expression) extends Condition
case class Eq(override val e1: Expression, override val e2: Expression) extends BinaryCondition(e1, e2)
case class NEq(override val e1: Expression, override val e2: Expression) extends BinaryCondition(e1, e2)
case class GrT(override val e1: Expression, override val e2: Expression) extends BinaryCondition(e1, e2)
case class LeT(override val e1: Expression, override val e2: Expression) extends BinaryCondition(e1, e2)
case class GoE(override val e1: Expression, override val e2: Expression) extends BinaryCondition(e1, e2)
case class LoE(override val e1: Expression, override val e2: Expression) extends BinaryCondition(e1, e2)
case class And(override val e1: Expression, override val e2: Expression) extends BinaryCondition(e1, e2)
case class Or(override val e1: Expression, override val e2: Expression) extends BinaryCondition(e1, e2)


// =====================
// Statements
// =====================

sealed abstract class Statement extends ASTNode 
case class ExpressionStmt(expr: Expression) extends Statement
case class Assignment(expr: Expression, value: Expression) extends Statement
case class Block(stmts: List[Opt[Statement]]) extends Statement
case class While(cond: Expression, body: Block) extends Statement
case class If(cond: Expression, s1: Block, s2: Option[Block]) extends Statement
case class Assert(cond: Expression) extends Statement

// functions
case class FuncDec(name: String, args: List[String], body: Block) extends Statement
sealed abstract class FuncDef
case class FDef(args: List[String], body: Block) extends FuncDef
case class FErr(msg: String) extends FuncDef

// classes
//case class ConstDec(name: String, expr: Expression)
case class ClassDec(name: String, superClass: String, fields: List[String], consts: Map[String, Expression], methods: List[Opt[FuncDec]]) extends Statement
abstract class ClassDef
case class VACDef(superClass: String, fields: List[String], consts: Map[String, Conditional[Value]], funcStore: VAFuncStore) extends ClassDef
case class PlainCDef(superClass: String, fields: List[String], consts: Map[String, Value], funcStore: PlainFuncStore) extends ClassDef
case class CErr(msg: String) extends ClassDef



