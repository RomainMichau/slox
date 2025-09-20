package com.rokim.lox

import cats.data.ValidatedNel
import cats.implicits.*

object Interpreter {
  case class InterpreterRuntimeError(token: Token, message: String)

  private def isTruthy(any: Any): Boolean = {
    if (any == null) false
    else any match {
      case bool: Boolean => bool
      case _ => true
    }
  }

  private def stringify(any: Any): String = {
    any match {
      case null => "nil"
      case d: Double if d.toString.endsWith(".0") => d.toString.dropRight(2)
      case d => d.toString
    }
  }


  def interprete(stmts: Seq[Stmt]) : ValidatedNel[InterpreterRuntimeError, Any] = {
    evaluate(stmts)
  }

  private def evaluate(stmts: Seq[Stmt]): ValidatedNel[InterpreterRuntimeError, Any] = {
    stmts.foldLeft[ValidatedNel[InterpreterRuntimeError, Any]](().validNel) { (acc, stmt) =>
      acc.andThen { _ =>
        stmt match {
          case Expression(expr) => evaluate(expr).map(_ => ())
          case Print(expr) => evaluate(expr).map { value =>
            println(stringify(value))
            ()
          }
        }
      }
    }
  }
  
  private def evaluate(expr: Expr): ValidatedNel[InterpreterRuntimeError, Any] = {
    expr match {
      case Literal(value) => value.validNel
      case Grouping(expr) => evaluate(expr)
      case Unary(operator, right) => operator match {
        case MINUS(_) => evaluate(right).map(r => -r.asInstanceOf[Double])
        case BANG(_) => evaluate(right).map(r => !isTruthy(r))
      }

      case Binary(leftEx, operator, rightEx) =>
        val leftAna = evaluate(leftEx)
        val rightAna = evaluate(rightEx)
        operator match {
          case op: MINUS => (leftAna, rightAna).tupled.andThen {
            case (l: Double, r: Double) => (l - r).validNel
            case (left, right) => InterpreterRuntimeError(op, s"Operands must be numbers for subtraction. Got ${left.getClass.getSimpleName} and ${right.getClass.getSimpleName}.").invalidNel
          }
          case op: STAR => (leftAna, rightAna).tupled.andThen {
            case (l: Double, r: Double) => (l * r).validNel
            case (left, right) => InterpreterRuntimeError(op, s"Operands must be numbers for multiplication. Got ${left.getClass.getSimpleName} and ${right.getClass.getSimpleName}.").invalidNel
          }
          case op: SLASH => (leftAna, rightAna).tupled.andThen {
            case (l: Double, r: Double) => (l / r).validNel
            case (left, right) => InterpreterRuntimeError(op, s"Operands must be numbers for division. Got ${left.getClass.getSimpleName} and ${right.getClass.getSimpleName}.").invalidNel
          }
          case op: PLUS => (leftAna, rightAna).tupled.andThen {
            case (l: String, r: String) => (l + r).validNel
            case (l: Double, r: Double) => (l + r).validNel
            case (l: Double, r: String) => (l.toString + r).validNel
            case (l: String, r: Double) => (l + r.toString).validNel
            case (left, right) => InterpreterRuntimeError(op, s"Operands must be two numbers or two strings for addition. Got ${left.getClass.getSimpleName} and ${right.getClass.getSimpleName}.").invalidNel
          }
          case op: GREATER => (leftAna, rightAna).tupled.andThen {
            case (l: Double, r: Double) => (l > r).validNel
            case (left, right) => InterpreterRuntimeError(op, s"Operands must be numbers for comparison. Got ${left.getClass.getSimpleName} and ${right.getClass.getSimpleName}.").invalidNel
          }
          case op: GREATER_EQUAL => (leftAna, rightAna).tupled.andThen {
            case (l: Double, r: Double) => (l >= r).validNel
            case (left, right) => InterpreterRuntimeError(op, s"Operands must be numbers for comparison. Got ${left.getClass.getSimpleName} and ${right.getClass.getSimpleName}.").invalidNel
          }
          case op: LESS => (leftAna, rightAna).tupled.andThen {
            case (l: Double, r: Double) => (l < r).validNel
            case (left, right) => InterpreterRuntimeError(op, s"Operands must be numbers for comparison. Got ${left.getClass.getSimpleName} and ${right.getClass.getSimpleName}.").invalidNel
          }
          case op: LESS_EQUAL => (leftAna, rightAna).tupled.andThen {
            case (l: Double, r: Double) => (l <= r).validNel
            case (left, right) => InterpreterRuntimeError(op, s"Operands must be numbers for comparison. Got ${left.getClass.getSimpleName} and ${right.getClass.getSimpleName}.").invalidNel
          }

          case _: BANG_EQUAL => (leftAna, rightAna).mapN((left, right) =>
            left != right)
          case _: EQUAL_EQUAL => (leftAna, rightAna).mapN((left, right) =>
            left == right)
        }
    }
  }
}