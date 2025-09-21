package com.rokim.lox

sealed trait Stmt {}

case class Expression(expr: Expr)                                  extends Stmt
case class Print(expr: Expr)                                       extends Stmt
case class Var(name: IDENTIFIER, initializer: Option[Expr] = None) extends Stmt
case class Block(statements: Seq[Stmt])                            extends Stmt
