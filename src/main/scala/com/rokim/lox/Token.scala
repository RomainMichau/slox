package com.rokim.lox

sealed trait Token {
  def lexeme: String
  def line: Int
}

// Single-character tokens
case class LEFT_PAREN(lexeme: String, line: Int) extends Token
case class RIGHT_PAREN(lexeme: String, line: Int) extends Token
case class LEFT_BRACE(lexeme: String, line: Int) extends Token
case class RIGHT_BRACE(lexeme: String, line: Int) extends Token
case class COMMA(lexeme: String, line: Int) extends Token
case class DOT(lexeme: String, line: Int) extends Token
case class MINUS(lexeme: String, line: Int) extends Token
case class PLUS(lexeme: String, line: Int) extends Token
case class SEMICOLON(lexeme: String, line: Int) extends Token
case class SLASH(lexeme: String, line: Int) extends Token
case class STAR(lexeme: String, line: Int) extends Token

// One or two character tokens
case class BANG(lexeme: String, line: Int) extends Token
case class BANG_EQUAL(lexeme: String, line: Int) extends Token
case class EQUAL(lexeme: String, line: Int) extends Token
case class EQUAL_EQUAL(lexeme: String, line: Int) extends Token
case class GREATER(lexeme: String, line: Int) extends Token
case class GREATER_EQUAL(lexeme: String, line: Int) extends Token
case class LESS(lexeme: String, line: Int) extends Token
case class LESS_EQUAL(lexeme: String, line: Int) extends Token

// Literals
case class IDENTIFIER(lexeme: String, line: Int) extends Token
case class STRING(lexeme: String, line: Int, literal: String) extends Token
case class NUMBER(lexeme: String, line: Int, literal: Double) extends Token

// Keywords
case class AND(lexeme: String, line: Int) extends Token
case class CLASS(lexeme: String, line: Int) extends Token
case class ELSE(lexeme: String, line: Int) extends Token
case class FALSE(lexeme: String, line: Int) extends Token
case class FUN(lexeme: String, line: Int) extends Token
case class FOR(lexeme: String, line: Int) extends Token
case class IF(lexeme: String, line: Int) extends Token
case class NIL(lexeme: String, line: Int) extends Token
case class OR(lexeme: String, line: Int) extends Token
case class PRINT(lexeme: String, line: Int) extends Token
case class RETURN(lexeme: String, line: Int) extends Token
case class SUPER(lexeme: String, line: Int) extends Token
case class THIS(lexeme: String, line: Int) extends Token
case class TRUE(lexeme: String, line: Int) extends Token
case class VAR(lexeme: String, line: Int) extends Token
case class WHILE(lexeme: String, line: Int) extends Token

case class EOF(lexeme: String, line: Int) extends Token
