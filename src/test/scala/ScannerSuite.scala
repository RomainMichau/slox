package com.rokim.lox

import cats.data.ValidatedNec
import cats.implicits._
import cats.data.Validated.Valid

class ScannerSuite extends munit.FunSuite {

  test("single character tokens") {
    val source = "(){},.-+;*"
    val result = Scanner.scan(source)

    result match {
      case Valid(tokens) =>
        assertEquals(tokens.size, 10)
        assertEquals(tokens.head, LEFT_PAREN(0))
        assertEquals(tokens(1), RIGHT_PAREN(0))
        assertEquals(tokens(2), LEFT_BRACE(0))
        assertEquals(tokens(3), RIGHT_BRACE(0))
        assertEquals(tokens(4), COMMA(0))
        assertEquals(tokens(5), DOT(0))
        assertEquals(tokens(6), MINUS(0))
        assertEquals(tokens(7), PLUS(0))
        assertEquals(tokens(8), SEMICOLON(0))
        assertEquals(tokens(9), STAR(0))
      case cats.data.Validated.Invalid(errors) =>
        fail(s"Expected valid tokens but got errors: $errors")
    }
  }

  test("two character tokens - equality") {
    val source = "!= == <= >="
    val result = Scanner.scan(source)

    result match {
      case Valid(tokens) =>
        assertEquals(tokens.size, 4)
        assertEquals(tokens.head, BANG_EQUAL(0))
        assertEquals(tokens(1), EQUAL_EQUAL(0))
        assertEquals(tokens(2), LESS_EQUAL(0))
        assertEquals(tokens(3), GREATER_EQUAL(0))
      case cats.data.Validated.Invalid(errors) =>
        fail(s"Expected valid tokens but got errors: $errors")
    }
  }

  test("single character versions of comparison operators") {
    val source = "! = < >"
    val result = Scanner.scan(source)

    result match {
      case Valid(tokens) =>
        assertEquals(tokens.size, 4)
        assertEquals(tokens.head, BANG(0))
        assertEquals(tokens(1), EQUAL(0))
        assertEquals(tokens(2), LESS(0))
        assertEquals(tokens(3), GREATER(0))
      case cats.data.Validated.Invalid(errors) =>
        fail(s"Expected valid tokens but got errors: $errors")
    }
  }

  test("operators at end of input") {
    val source = "="
    val result = Scanner.scan(source)

    result match {
      case Valid(tokens) =>
        assertEquals(tokens.size, 1)
        assertEquals(tokens.head, EQUAL(0))
      case cats.data.Validated.Invalid(errors) =>
        fail(s"Expected valid tokens but got errors: $errors")
    }
  }

  test("slash and comments") {
    val source = "/ // this is a comment\n+"
    val result = Scanner.scan(source)

    result match {
      case Valid(tokens) =>
        assertEquals(tokens.size, 2)
        assertEquals(tokens.head, SLASH(0))
        assertEquals(tokens(1), PLUS(1))
      case cats.data.Validated.Invalid(errors) =>
        fail(s"Expected valid tokens but got errors: $errors")
    }
  }

  test("whitespace handling") {
    val source = "( ) \n { }"
    val result = Scanner.scan(source)

    result match {
      case Valid(tokens) =>
        assertEquals(tokens.size, 4)
        assertEquals(tokens.head, LEFT_PAREN(0))
        assertEquals(tokens(1), RIGHT_PAREN(0))
        assertEquals(tokens(2), LEFT_BRACE(1))
        assertEquals(tokens(3), RIGHT_BRACE(1))
      case cats.data.Validated.Invalid(errors) =>
        fail(s"Expected valid tokens but got errors: $errors")
    }
  }

  test("string literals") {
    val source = "\"hello world\""
    val result = Scanner.scan(source)

    result match {
      case Valid(tokens) =>
        assertEquals(tokens.size, 1)
        assertEquals(tokens.head, STRING("hello world", 0, "\"hello world\""))
      case cats.data.Validated.Invalid(errors) =>
        fail(s"Expected valid tokens but got errors: $errors")
    }
  }

  test("multiline string literals") {
    val source = "\"hello\nworld\""
    val result = Scanner.scan(source)

    result match {
      case Valid(tokens) =>
        assertEquals(tokens.size, 1)
        assertEquals(tokens.head, STRING("hello\nworld", 0, "\"hello\nworld\""))
      case cats.data.Validated.Invalid(errors) =>
        fail(s"Expected valid tokens but got errors: $errors")
    }
  }

  test("unterminated string") {
    val source = "\"unterminated"
    val result = Scanner.scan(source)

    result match {
      case Valid(tokens) =>
        fail(s"Expected error for unterminated string but got tokens: $tokens")
      case cats.data.Validated.Invalid(errors) =>
        assertEquals(errors.size, 1)
        assert(errors.head.message.contains("Unterminated string"))
        assertEquals(errors.head.line, 0)
    }
  }

  test("empty string") {
    val source = ""
    val result = Scanner.scan(source)

    result match {
      case Valid(tokens) =>
        assertEquals(tokens.size, 0)
      case cats.data.Validated.Invalid(errors) =>
        fail(s"Expected empty token list but got errors: $errors")
    }
  }

  test("line counting") {
    val source = "(\n)\n{"
    val result = Scanner.scan(source)

    result match {
      case Valid(tokens) =>
        assertEquals(tokens.size, 3)
        assertEquals(tokens.head.line, 0)
        assertEquals(tokens(1).line, 1)
        assertEquals(tokens(2).line, 2)
      case cats.data.Validated.Invalid(errors) =>
        fail(s"Expected valid tokens but got errors: $errors")
    }
  }

  test("int number") {
    val source = "123"
    val result = Scanner.scan(source)

    result match {
      case Valid(NUMBER("123", 0, 123) :: Nil) =>
      case b                                   => fail(s"Expected valid number not $b")
    }
  }

  test("frac number") {
    val source = "123.456"
    val result = Scanner.scan(source)

    result match {
      case Valid(NUMBER("123.456", 0, 123.456) :: Nil) =>
      case b                                           => fail(s"Expected valid number not $b")
    }
  }

  test("keywords") {
    val source = "if else var"
    val result = Scanner.scan(source)

    result match {
      case Valid(tokens) =>
        assertEquals(tokens.size, 3)
        assertEquals(tokens.head, IF(0))
        assertEquals(tokens(1), ELSE(0))
        assertEquals(tokens(2), VAR_TKN(0))
      case cats.data.Validated.Invalid(errors) =>
        fail(s"Expected valid tokens but got errors: $errors")
    }
  }

  test("identifiers") {
    val source = "myVar someFunction"
    val result = Scanner.scan(source)

    result match {
      case Valid(tokens) =>
        assertEquals(tokens.size, 2)
        assertEquals(tokens.head, IDENTIFIER("myVar", 0))
        assertEquals(tokens(1), IDENTIFIER("someFunction", 0))
      case cats.data.Validated.Invalid(errors) =>
        fail(s"Expected valid tokens but got errors: $errors")
    }
  }

  test("mixed keywords and identifiers") {
    val source = "if myVar else"
    val result = Scanner.scan(source)

    result match {
      case Valid(tokens) =>
        assertEquals(tokens.size, 3)
        assertEquals(tokens.head, IF(0))
        assertEquals(tokens(1), IDENTIFIER("myVar", 0))
        assertEquals(tokens(2), ELSE(0))
      case cats.data.Validated.Invalid(errors) =>
        fail(s"Expected valid tokens but got errors: $errors")
    }
  }
}
