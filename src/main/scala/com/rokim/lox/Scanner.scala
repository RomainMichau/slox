package com.rokim.lox

import cats.data.{ValidatedNec, ValidatedNel}
import cats.implicits.{catsSyntaxValidatedId, catsSyntaxValidatedIdBinCompat0}

object Scanner {

  private def keywordOrIdentifier(lexeme: String, line: Int): Token = lexeme match {
    case "and" => AND(lexeme, line)
    case "class" => CLASS(lexeme, line)
    case "else" => ELSE(lexeme, line)
    case "false" => FALSE(lexeme, line)
    case "for" => FOR(lexeme, line)
    case "fun" => FUN(lexeme, line)
    case "if" => IF(lexeme, line)
    case "nil" => NIL(lexeme, line)
    case "or" => OR(lexeme, line)
    case "print" => PRINT(lexeme, line)
    case "return" => RETURN(lexeme, line)
    case "super" => SUPER(lexeme, line)
    case "this" => THIS(lexeme, line)
    case "true" => TRUE(lexeme, line)
    case "var" => VAR(lexeme, line)
    case "while" => WHILE(lexeme, line)
    case _ => IDENTIFIER(lexeme, line)
  }

  def scan(source: String): ValidatedNel[LoxError, Seq[Token]] = {
    val sourceList = source.toList

    def loop(source: List[Char], acc: Seq[Token], line: Int): ValidatedNel[LoxError, Seq[Token]] = source match {
      case '(' :: tail => loop(tail, LEFT_PAREN("(", line) +: acc, line)
      case ')' :: tail => loop(tail, RIGHT_PAREN(")", line) +: acc, line)
      case '{' :: tail => loop(tail, LEFT_BRACE("{", line) +: acc, line)
      case '}' :: tail => loop(tail, RIGHT_BRACE("}", line) +: acc, line)
      case ',' :: tail => loop(tail, COMMA(",", line) +: acc, line)
      case '.' :: tail => loop(tail, DOT(".", line) +: acc, line)
      case '-' :: tail => loop(tail, MINUS("-", line) +: acc, line)
      case '+' :: tail => loop(tail, PLUS("+", line) +: acc, line)
      case ';' :: tail => loop(tail, SEMICOLON(";", line) +: acc, line)
      case '*' :: tail => loop(tail, STAR("*", line) +: acc, line)
      case '!' :: tail => tail match {
        case '=' :: rest => loop(rest, BANG_EQUAL("!=", line) +: acc, line)
        case _ => loop(tail, BANG("!", line) +: acc, line)
      }
      case '=' :: tail => tail match {
        case '=' :: rest => loop(rest, EQUAL_EQUAL("==", line) +: acc, line)
        case _ => loop(tail, EQUAL("=", line) +: acc, line)
      }
      case '<' :: tail => tail match {
        case '=' :: rest => loop(rest, LESS_EQUAL("<=", line) +: acc, line)
        case _ => loop(tail, LESS("<", line) +: acc, line)
      }
      case '>' :: tail => tail match {
        case '=' :: rest => loop(rest, GREATER_EQUAL(">=", line) +: acc, line)
        case _ => loop(tail, GREATER(">", line) +: acc, line)
      }
      case '/' :: tail => tail match {
        case '/' :: rest =>
          val (comment, rest2) = rest.span(_ != '\n')
          loop(rest2, acc, line)
        case _ => loop(tail, SLASH("\\", line) +: acc, line)
      }
      case ' ' :: tail => loop(tail, acc, line)
      case '\r' :: tail => loop(tail, acc, line)
      case '\t' :: tail => loop(tail, acc, line)
      case '\n' :: tail => loop(tail, acc, line + 1)
      case '"' :: tail => tail.span(_ != '"') match {
        case (content, '"' :: rest) =>
          val nline = line + content.count(_ == '\n')
          loop(rest, STRING(content.mkString, line, s"\"${content.mkString}\"") +: acc, nline)
        case _ => LoxError(line, "", "Unterminated string").invalidNel
      }
      case x::tail if x.isDigit =>
        val (integer, rest) = tail.span(_.isDigit)
        val (fractional, next) = if(rest.headOption.contains('.')) rest.tail.span(_.isDigit) else (List.empty, rest)
        val nb = s"$x${integer.mkString}" + (if(fractional.nonEmpty) s".${fractional.mkString}" else "")
        loop(next, NUMBER(nb, line, nb.toDouble) +: acc, line)
      case x::tail if x.isLetter => tail.span(_.isLetter) match {
        case (letter,rest) =>
          loop(rest, keywordOrIdentifier(s"$x${letter.mkString}", line) +: acc, line)
      }
      case Nil => acc.validNel
      case c :: tail => LoxError(line, "", s"Unexpected character: $c").invalidNel
    }

    loop(source.toList, Seq.empty, 0).map(_.reverse)
  }
}
