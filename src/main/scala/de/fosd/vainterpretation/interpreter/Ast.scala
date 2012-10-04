package de.fosd.vainterpretation.interpreter

import de.fosd.typechef.conditional._


sealed abstract class ASTNode {
//  override def toString = {
//    SourceCodePrettyPrinter.prettyPrintNode(this).mkString
//  }
}

// =====================
// Statements
// =====================

sealed abstract class Stmt extends ASTNode 
case class Assign(expr: Expr, value: Conditional[Expr]) extends Stmt
case class Block(stmts: List[Opt[Stmt]]) extends Stmt
case class While(cond: Conditional[Expr], body: Block) extends Stmt
case class If(cond: Conditional[Expr], then: Block, els: Option[Block]) extends Stmt
case class ExprStmt(expr: Expr) extends Stmt
case class Assert(cond: Expr) extends Stmt

// =====================
// Expressions
// =====================

sealed abstract class Expr extends ASTNode
case object Null extends Expr
case class Num(n: Int) extends Expr
case class Var(name: String) extends Expr
case class Par(e: Expr) extends Expr
case class Add(e1: Expr, e2: Expr) extends Expr
case class Sub(e1: Expr, e2: Expr) extends Expr
case class Mul(e1: Expr, e2: Expr) extends Expr
case class Div(e1: Expr, e2: Expr) extends Expr

case class Bool(b: Boolean) extends Expr
case class Neg(e: Expr) extends Expr
case class Eq(e1: Expr, e2: Expr) extends Expr
case class NEq(e1: Expr, e2: Expr) extends Expr 
case class GrT(e1: Expr, e2: Expr) extends Expr
case class LeT(e1: Expr, e2: Expr) extends Expr
case class GoE(e1: Expr, e2: Expr) extends Expr
case class LoE(e1: Expr, e2: Expr) extends Expr
case class And(e1: Expr, e2: Expr) extends Expr
case class Or(e1: Expr, e2: Expr) extends Expr


// functions
case class Call(fname: String, args: List[Opt[Expr]]) extends Expr

// classes
case class New(name: String, args: List[Opt[Expr]]) extends Expr
case class Field(expr: Expr, name: String) extends Expr
case class MethodCall(expr: Expr, call: Call) extends Expr



// functions
case class FuncDec(name: String, args: List[Opt[String]], body: Block) extends Stmt

sealed abstract class FuncDef
case class FDef(args: List[Opt[String]], body: Block) extends FuncDef
case class FErr(msg: String) extends FuncDef

// classes
case class ClassDec(name: String, args: List[Opt[String]], superClass: String, consts: List[Opt[Assign]], fields: List[Opt[Assign]], methods: List[Opt[FuncDec]]) extends Stmt

abstract class ClassDef
case class VACDef(args: List[Opt[String]], superClass: String, fields: List[Opt[Assign]], constStore: VAStore, funcStore: VAFuncStore) extends ClassDef
case class PlainCDef(args: List[String], superClass: String, fields: List[Assign], constStore: PlainStore, funcStore: PlainFuncStore) extends ClassDef
case class CErr(msg: String) extends ClassDef



