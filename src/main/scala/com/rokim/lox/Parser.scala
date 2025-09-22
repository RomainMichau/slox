package com.rokim.lox

import cats.data.Validated.Valid
import cats.data.{Validated, ValidatedNel}
import cats.implicits.*
import com.rokim.lox.Parser.{ExpressionParsing, ParserError, StmtParsing, StmtsParsing}

import scala.annotation.tailrec

object Parser {
  case class ParserError(token: Token, message: String)

  type ExpressionParsing = ValidatedNel[ParserError, Expr]
  type StmtsParsing      = ValidatedNel[ParserError, Seq[Stmt]]
  type StmtParsing       = ValidatedNel[ParserError, Stmt]
}

class Parser(tokens: Seq[Token]) {
  private type TokenMatcher[T] = PartialFunction[Token, Option[T]]
  private var current = 0

  def parserError(message: String): ParserError =
    ParserError(peek, message)

  // format: off
  /**
   * Grammar Rules:
   *
   * program     → statement* EOF ;
   * declaration → varDecl
   *             | statement ;
   * varDecl     → "var" IDENTIFIER ( "=" expression )? ";" ;
   * statement   → exprStmt
   *             | ifStmt
   *             | printStmt
   *             | block
   *             | whileStmt
   *             | forStmt;
   * forStmt        → "for" "(" ( varDecl | exprStmt | ";" )
   * expression? ";"
   * expression? ")" statement ;
   * whileStmt   → "while" "(" expression ")" statement ;
   * ifStmt      → "if" "(" expression ")" statement
   *               ( "else" statement )? ;
   * block       → "{" declaration* "}" ;
   * exprStmt    → expression ";" ;
   * printStmt   → "print" expression ";" ;
   * expression  → assignment ;
   * assignment  → IDENTIFIER "=" assignment
   *             | logic_or ;
   * logic_or    → logic_and ( "or" logic_and )* ;
   * logic_and   → equality ( "and" equality )* ;
   * equality    → comparison ( ("!=" | "==") comparison )* ;
   * comparison  → term ( (">" | ">=" | "<" | "<=") term )* ;
   * term        → factor ( ("-" | "+") factor )* ;
   * factor      → unary ( ("/" | "*") unary )* ;
   * unary       → ("!" | "-") unary
   *             | primary ;
   * primary     → NUMBER | STRING | "true" | "false" | "nil"
   *             | "(" expression ")" | IDENTIFIER ;
   */
  // format: on

  def parse(): StmtsParsing = {
    def loop(): StmtsParsing = {
      if (!isAtEnd) {
        val dec = declaration().leftMap { e =>
          synchronize()
          e
        }

        (loop(), dec).mapN { case (l, e) => e +: l }
      } else {
        Nil.validNel
      }
    }

    loop()
  }

  private def synchronize(): Unit = {
    @tailrec
    def loop(): Unit = {
      advance()
      (previous(), peek) match {
        case (_: SEMICOLON, _)                                                                          =>
        case (_, _: CLASS | _: FUN | _: VAR_TKN | _: FOR | _: IFTkn | _: WHILE_TKN | _: PRINT_TKN | _: RETURN) =>
        case _                                                                                          => if (!isAtEnd) loop()
      }
    }

    loop()
  }

  private def isAtEnd: Boolean = peek match {
    case EOF(_) => true
    case _      => false
  }

  private def peek = tokens(current)

  private def previous() = tokens(current - 1)

  private def check[T](matcher: TokenMatcher[T]): Option[T] =
    if (isAtEnd) None
    else matcher.applyOrElse(peek, _ => None)

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

  private def consume[T](matcher: TokenMatcher[T], expect: String): ValidatedNel[ParserError, T] = {
    check(matcher) match {
      case Some(t) =>
        advance()
        Valid(t)
      case None => parserError(expect).invalidNel
    }
  }

  private def consumeSemicolon(): ValidatedNel[ParserError, SEMICOLON] = {
    consume({ case t: SEMICOLON => Some(t) }, "Expect ';'")
  }

  private def declaration(): StmtParsing = {
    matchToken { case _: VAR_TKN => Some(()) }
      .map(_ => varDeclaration())
      .getOrElse(statement())
  }

  private def statement(): StmtParsing = {
    matchToken { case a @ (_: PRINT_TKN | _: LEFT_BRACE | _: IFTkn |_: WHILE_TKN |_: FOR) => Some(a) } match {
      case Some(PRINT_TKN(_))  => printStatement()
      case Some(LEFT_BRACE(_)) => block()
      case Some(IFTkn(_)) => ifStmt()
      case Some(WHILE_TKN(_)) => whileStmt()
      case Some(FOR(_)) => forStmt()
      case _                   => expressionStatement()
    }
  }

  private def forStmt(): StmtParsing = {
    def getVarDeclOrExprStmt(): StmtParsing = {
      check({case t: Var => Some(t)}) match {
        case None => expressionStatement()
        case Some(_) => varDeclaration()
      }
    }

    def getLastPart[T](x: () => T): Option[T] = {
      val res = check({ case t: RIGHT_PAREN => Some(t) }) match {
        case None => Some(x())
        case Some(_) => None
      }
      consume({ case t: RIGHT_PAREN => Some(t) }, "Expect { after `for`")
      res
    }

    def getForPart[T](x: () => T): Option[T] = {
      val res = matchToken({ case t: SEMICOLON => Some(t) }) match {
        case None => Some(x())
        case Some(_) => None
      }
      consumeSemicolon()
      res
    }
    consume({case t: LEFT_PAREN => Some(t)}, "expect left parent after for")
      .andThen{_ =>
        val maybeInit = getForPart(getVarDeclOrExprStmt)
        val maybeCond = getForPart(expression)
        val maybeIncrement = getLastPart(expression)


    }
  }

  private def whileStmt(): StmtParsing = {
    consume({ case t: LEFT_PAREN => Some(t)}, "expect ( after `while`").andThen{ _ =>
      expression().andThen{cond =>
        consume({ case t: RIGHT_PAREN => Some(t)}, "expect ) after `while`").andThen{_ =>
          statement().map{ st =>
            While(cond, st)
          }
        }
      }
    }
  }


  private def ifStmt(): StmtParsing = {
    consume({ case t: LEFT_PAREN => Some(t)}, "expect ( after `if`")
      .andThen{_ =>
        expression().andThen{condition =>
          consume({ case t: RIGHT_PAREN => Some(t)}, "expect ) after `if`")
            .andThen{_ =>
              statement().andThen { ifTrue =>
                matchToken({case t: ELSE => Some(t)}).map{_ =>
                  statement().map{
                    elseS => If(condition, ifTrue, Some(elseS))
                  }
                }.getOrElse(If(condition, ifTrue, None).validNel)
              }
          }
        }
      }
  }

  private def block(): StmtParsing = {
    def loop(): ValidatedNel[ParserError, Block] = {
      matchToken { case t: RIGHT_BRACE => Some(t) }
        .map { _ =>
          Block(Seq.empty).validNel
        }
        .getOrElse(declaration().andThen(dec => loop().map(l => l.copy(statements = dec +: l.statements))))
    }

    loop().map(r => r.copy(r.statements))
  }

  private def printStatement(): StmtParsing = {
    expression().andThen { expr =>
      consume(
        { case _: SEMICOLON =>
          Some(SEMICOLON)
        },
        "Expect ';' after value."
      ).map(_ => Print(expr))
    }
  }

  private def expressionStatement(): StmtParsing = {
    expression().andThen(expr =>
      consume({ case t: SEMICOLON => Some(t) }, "Expect ';' after value._").map(_ => Expression(expr))
    )
  }

  private def expression(): ExpressionParsing =
    assignment()

  private def assignment(): ExpressionParsing = {
    or().andThen { expr =>
      matchToken { case e: EQUAL => Some(e) }
        .map { equals =>
          assignment().andThen { value =>
            expr match {
              case Variable(name) => Assign(name, value).validNel
              case _              => ParserError(equals, "Invalid assignement").invalidNel
            }
          }
        }
        .getOrElse(expr.validNel)
    }
  }

  private def or(): ExpressionParsing = {
    def loop(left: Expr): ExpressionParsing = {
      matchToken({ case t: OR => Some(t) }).map { or =>
        and().andThen { right =>
          loop(Logical(left, or, right))
        }
      }.getOrElse(left.validNel)
    }
    and().andThen(loop)
  }

  private def and(): ExpressionParsing = {
    def loop(left: Expr): ExpressionParsing = {
      matchToken({ case t: AND => Some(t) }).map { and =>
        equality().andThen { right =>
          loop(Logical(left, and, right))
        }
      }.getOrElse(left.validNel)
    }
    equality().andThen(loop)
    }


  private def varDeclaration(): StmtParsing = {
    consume({ case t: IDENTIFIER => Some(t) }, "expect var name")
      .andThen { ident =>
        matchToken { case _: EQUAL => Some(()) } match {
          case Some(_) =>
            expression().map(init => Var(ident, Some(init)))
          case None => Var(ident).validNel
        }
      }
      .andThen(stmt =>
        consume({ case t: SEMICOLON => Some(t) }, "Expect ';' after declaration.")
          .as(stmt)
      )
  }

  private def equality(): ExpressionParsing = {
    val matcher: TokenMatcher[BinaryOperatorToken] = { case a @ (_: BANG_EQUAL | _: EQUAL_EQUAL) =>
      Some(a)
    }

    def loop(expr: Expr): ExpressionParsing = {
      matchToken(matcher) match {
        case Some(operator) => comparison().andThen(right => loop(Binary(expr, operator, right)))
        case None           => expr.validNel
      }
    }

    comparison().andThen(x => loop(x))
  }

  private def comparison(): ExpressionParsing = {
    val matcher: TokenMatcher[BinaryOperatorToken] = {
      case a @ (_: GREATER | _: GREATER_EQUAL | _: LESS | _: LESS_EQUAL) => Some(a)
    }

    def loop(expr: Expr): ExpressionParsing = {
      matchToken(matcher) match {
        case Some(operator) => term().andThen(right => loop(Binary(expr, operator, right)))
        case None           => expr.validNel
      }
    }

    term().andThen(x => loop(x))
  }

  private def term(): ExpressionParsing = {
    val matcher: TokenMatcher[BinaryOperatorToken] = { case a @ (_: MINUS | _: PLUS) =>
      Some(a)
    }

    def loop(expr: Expr): ExpressionParsing = {
      matchToken(matcher) match {
        case Some(operator) => factor().andThen(right => loop(Binary(expr, operator, right)))
        case None           => expr.validNel
      }
    }

    factor().andThen(x => loop(x))
  }

  private def factor(): ExpressionParsing = {
    val matcher: TokenMatcher[BinaryOperatorToken] = { case a @ (_: STAR | _: SLASH) =>
      Some(a)
    }

    def loop(expr: Expr): ExpressionParsing = {
      matchToken(matcher) match {
        case Some(operator) => unary().andThen(right => loop(Binary(expr, operator, right)))
        case None           => expr.validNel
      }
    }

    unary().andThen(x => loop(x))
  }

  private def unary(): ExpressionParsing = {
    val matcher: TokenMatcher[UnaryOperatorToken] = { case a @ (_: BANG | _: MINUS) =>
      Some(a)
    }

    matchToken(matcher) match {
      case Some(operator) => unary().map(right => Unary(operator, right))
      case None           => primary()
    }
  }

  private def primary(): ExpressionParsing = {
    val matcher: TokenMatcher[Token] = {
      case a @ (_: NUMBER | _: STRING | _: FALSE | _: TRUE | _: NIL | _: LEFT_PAREN | _: IDENTIFIER) => Some(a)
    }

    matchToken(matcher) match {
      case Some(token) =>
        token match {
          case NUMBER(_, _, value) => Literal(value).validNel
          case STRING(_, _, value) => Literal(value).validNel
          case _: FALSE            => Literal(false).validNel
          case _: TRUE             => Literal(true).validNel
          case _: NIL              => Literal(null).validNel
          case i: IDENTIFIER       => Variable(i).validNel
          case _: LEFT_PAREN =>
            expression().andThen(expr =>
              consume({ case a: RIGHT_PAREN => Some(a) }, "Expect ')' after expression.").map(_ => Grouping(expr))
            )
          case _ => parserError("Expect expression ?").invalidNel
        }
      case None => parserError("Expect expression").invalidNel
    }
  }
}

//object Test {
//  import cats.implicits._
//  def main(args: Array[ParserError]): Unit = {
//    val v1 = Validated.validNel(1)
//    val v2 = Validated.validNel(2)
//    val i1 = "error1".invalidNel
//    val i2 = "error2".invalidNel
//    println((v1, i2).mapN{case(a, b) => a.toString + b.toString})
//  }
//}
