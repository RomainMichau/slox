package com.rokim.lox

import cats.data.{NonEmptyList, Validated, ValidatedNel}
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
    val env = Environment()
    execute(stmts, env)
  }

  private def execute(stmt: Stmt, env: Environment): ValidatedNel[InterpreterRuntimeError, Any] = {
    execute(Seq(stmt), env)
  }

  private def execute(stmts: Seq[Stmt], env: Environment): ValidatedNel[InterpreterRuntimeError, Any] = {
    stmts.foldLeft[ValidatedNel[InterpreterRuntimeError, Any]](().validNel) { (acc, stmt) =>
      acc.andThen { _ =>
        stmt match {
          case Expression(expr) => evaluate(expr, env).map(_ => ())
          case Print(expr) => 
            evaluate(expr, env).map { value =>
            println(stringify(value))
            ()
          }
          case Var(name, value) =>
            value match {
              case None => env.define(name, null).validNel
              case Some(v) => evaluate(v, env).map(env.define(name, _))
            }
          case b: Block => executeBlock(b, env)
        }
      }
    }
  }

  private def executeBlock(block: Block, parentEnv: Environment): ValidatedNel[InterpreterRuntimeError, Any] = {
    val newEnv = parentEnv.childEnv()
    val stmts = block.statements
    stmts.headOption match {
      case Some(head) =>     stmts.tail.foldLeft(execute(head, newEnv)){case (acc, stmt) => acc.andThen(_ => execute(stmt, newEnv))}
      case None => ().validNel
    }
  }
  
  private def evaluate(expr: Expr, env: Environment): ValidatedNel[InterpreterRuntimeError, Any] = {
    expr match {
      case Literal(value) => value.validNel
      case Grouping(expr) => evaluate(expr, env)
      case Unary(operator, right) => operator match {
        case MINUS(_) => evaluate(right, env).map(r => -r.asInstanceOf[Double])
        case BANG(_) => evaluate(right, env).map(r => !isTruthy(r))
      }

      case Binary(leftEx, operator, rightEx) =>
        val leftAna = evaluate(leftEx, env)
        val rightAna = evaluate(rightEx, env)
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
      case Variable(name) => 
        env.get(name) match {
        case Some(value) => value.validNel
        case None => InterpreterRuntimeError(name, s"Unkown var ${name.lexeme}").invalidNel
      }
      case Assign(name, valueExpr) => evaluate(valueExpr, env).andThen{value =>
        env.assign(name, value) match {
          case _: true => value.validNel
          case _: false => InterpreterRuntimeError(name, s"Unkown var ${name.lexeme}").invalidNel
        }
      }
    }
  }
}