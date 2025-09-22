package com.rokim.lox

sealed trait Stmt {}

case class Expression(expr: Expr)                                  extends Stmt
case class Print(expr: Expr)                                       extends Stmt
case class Var(name: IDENTIFIER, initializer: Option[Expr] = None) extends Stmt
case class Block(statements: Seq[Stmt])                            extends Stmt
case class If(condition: Expr, trueBranch: Stmt, falseBranch: Option[Stmt]) extends Stmt
case class While(condition: Expr, body:Stmt) extends Stmt