package com.rokim.lox

import cats.implicits.*
import cats.data.Validated.Valid
import cats.data.ValidatedNel
import com.rokim.lox.Parser.{ExpressionParsing, ParserError, StmtParsing, StmtsParsing}

import scala.annotation.tailrec

object Parser {
  type ParserError = String
  type ExpressionParsing = ValidatedNel[ParserError, Expr]
  type StmtsParsing = ValidatedNel[ParserError, Seq[Stmt]]
  type StmtParsing = ValidatedNel[ParserError, Stmt]
}

class Parser(tokens: Seq[Token]) {
  private type TokenMatcher[T] = PartialFunction[Token, Option[T]]
  private var current = 0

  /**
   * program        → statement* EOF ;
   * statement      → exprStmt
   * | printStmt ;
   * exprStmt       → expression ";" ;
   * printStmt      → "print" expression ";" ;
   * expression → equality;
   * equality → comparison(("!=" | "==") comparison) *;
   * comparison → term((">" | ">=" | "<" | "<=") term) *;
   * term → factor(("-" | "+") factor) *;
   * factor → unary(("/" | "*") unary) *;
   * unary → ("!" | "-") unary
   * | primary;
   * primary → NUMBER | STRING | "true" | "false" | "nil"
   * | "(" expression ")";
   * */


  def parse(): StmtsParsing = {
    def loop(): StmtsParsing = {
      if(!isAtEnd) {
        statement().andThen(s => loop().map(l => s +: l))
      } else Nil.validNel
    }
    loop().map(_.reverse)
  }

  private def synchronize(): Unit = {
    @tailrec
    def loop(): Unit = {
      advance()
      (previous(), peek) match {
        case (_: SEMICOLON, _) =>
        case (_, _: CLASS | _: FUN | _: VAR | _: FOR | _: IF | _: WHILE | _: PRINT | _: RETURN) =>
        case _ => if (!isAtEnd) loop()
      }
    }

    loop()
  }

  private def isAtEnd: Boolean = peek match {
    case EOF(_) => true
    case _ => false
  }

  private def peek = tokens(current)

  private def previous() = tokens(current - 1)

  private def check[T](matcher: TokenMatcher[T]): Option[T] = {
    if (isAtEnd) None
    else matcher.applyOrElse(peek, _ => None)
  }

  private def advance(): Token = {
    if (!isAtEnd) current += 1
    previous()
  }

  private def matchToken[T](matcher: TokenMatcher[T]): Option[T] = {
    check(matcher).map { x =>
      advance()
      x
    }
  }

  private def consume[T](matcher: TokenMatcher[T], expect: String): ValidatedNel[ParserError, Unit] = {
    check(matcher) match {
      case Some(_) => advance()
        Valid(())
      case None => expect.invalidNel
    }
  }

  private def statement(): StmtParsing = {
    matchToken({ case a: PRINT => Some(a) }).map(_ => printStatement())
      .getOrElse(expressionStatement())
  }

  private def printStatement(): StmtParsing = {
    expression().andThen { expr =>
      consume({
        case t: SEMICOLON => Some(SEMICOLON)
      }, "Expect ';' after value.").map(_ => Print(expr))
    }
  }

  private def expressionStatement(): StmtParsing = {
    expression().andThen(expr =>
      consume({case t: SEMICOLON => Some(t)}, "Expect ';' after value.").map(_ =>
      Expression(expr)
    )
    )
  }

  private def expression(): ExpressionParsing = {
    equality()
  }

  private def equality(): ExpressionParsing = {
    val matcher: TokenMatcher[BinaryOperatorToken] = {
      case a@(_: BANG_EQUAL | _: EQUAL_EQUAL) => Some(a)
    }

    def loop(expr: Expr): ExpressionParsing = {
      matchToken(matcher) match {
        case Some(operator) => comparison().andThen(right => loop(Binary(expr, operator, right)))
        case None => expr.validNel
      }
    }

    comparison().andThen(x => loop(x))
  }

  private def comparison(): ExpressionParsing = {
    val matcher: TokenMatcher[BinaryOperatorToken] = {
      case a@(_: GREATER | _: GREATER_EQUAL | _: LESS | _: LESS_EQUAL) => Some(a)
    }

    def loop(expr: Expr): ExpressionParsing = {
      matchToken(matcher) match {
        case Some(operator) => term().andThen(right => loop(Binary(expr, operator, right)))
        case None => expr.validNel
      }
    }

    term().andThen(x => loop(x))
  }

  private def term(): ExpressionParsing = {
    val matcher: TokenMatcher[BinaryOperatorToken] = {
      case a@(_: MINUS | _: PLUS) => Some(a)
    }

    def loop(expr: Expr): ExpressionParsing = {
      matchToken(matcher) match {
        case Some(operator) => factor().andThen(right => loop(Binary(expr, operator, right)))
        case None => expr.validNel
      }
    }

    factor().andThen(x => loop(x))
  }

  private def factor(): ExpressionParsing = {
    val matcher: TokenMatcher[BinaryOperatorToken] = {
      case a@(_: STAR | _: SLASH) => Some(a)
    }

    def loop(expr: Expr): ExpressionParsing = {
      matchToken(matcher) match {
        case Some(operator) => unary().andThen(right => loop(Binary(expr, operator, right)))
        case None => expr.validNel
      }
    }

    unary().andThen(x => loop(x))
  }

  private def unary(): ExpressionParsing = {
    val matcher: TokenMatcher[UnaryOperatorToken] = {
      case a@(_: BANG | _: MINUS) => Some(a)
    }

    matchToken(matcher) match {
      case Some(operator) => unary().map(right => Unary(operator, right))
      case None => primary()
    }
  }

  private def primary(): ExpressionParsing = {
    val matcher: TokenMatcher[Token] = {
      case a@(_: NUMBER | _: STRING | _: FALSE | _: TRUE | _: NIL | _: LEFT_PAREN) => Some(a)
    }

    matchToken(matcher) match {
      case Some(token) => token match {
        case NUMBER(_, _, value) => Literal(value).validNel
        case STRING(_, _, value) => Literal(value).validNel
        case _: FALSE => Literal(false).validNel
        case _: TRUE => Literal(true).validNel
        case _: NIL => Literal(null).validNel
        case _: LEFT_PAREN =>
          expression().andThen(expr =>
            consume({ case a: RIGHT_PAREN => Some(a) }, "Expect ')' after expression.").map(_ => Grouping(expr))
          )
      }
      case None => "Expect expression".invalidNel
    }
  }
}
