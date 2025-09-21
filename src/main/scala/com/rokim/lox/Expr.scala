package com.rokim.lox

sealed trait Expr {}

case class Binary(left: Expr, operator: BinaryOperatorToken, right: Expr) extends Expr

case class Grouping(expr: Expr) extends Expr

case class Literal(value: Any) extends Expr

case class Unary(operator: UnaryOperatorToken, right: Expr) extends Expr

case class Assign(name: IDENTIFIER, value: Expr) extends Expr

case class Variable(name: IDENTIFIER) extends Expr

object ExprPrinter {
  private def parenthesize(name: String, expr: Expr*): String = {
    s"($name ${expr.map(print).mkString(" ")})"
  }

  def print(expr: Expr): String = expr match
    case Binary(left, token, right) => parenthesize(token.lexeme, left, right)
    case Grouping(expr) => parenthesize("group", expr)
    case Literal(value) => value.toString
    case Unary(operator, right) => parenthesize(operator.lexeme, right)
}




object MainTest {
  def main(args: Array[String]): Unit = {
    val expr = Binary(Unary(
      MINUS(0), Literal(123)
    ), STAR(0), Grouping(Literal(45.67)))
    println(ExprPrinter.print(expr))
  }
}