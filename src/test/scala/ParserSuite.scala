package com.rokim.lox

import cats.data.Validated.{Invalid, Valid}
import cats.data.ValidatedNel
import com.rokim.lox.Parser.ParserError
import cats.implicits.*

class ParserSuite extends munit.FunSuite {

  private def parseExpression(source: String): ValidatedNel[ParserError, Expr] = {
    Scanner.scan(source + ";") match {
      case Valid(tokens) =>
        val parser = new Parser(tokens :+ EOF(tokens.lastOption.map(_.line).getOrElse(1)))
        parser.parse() match {
          case Valid(stmts) if stmts.nonEmpty =>
            stmts.head match {
              case Expression(expr) => Valid(expr)
              case other => ParserError(EOF(1), s"Expected expression statement, got: $other").invalidNel
            }
          case Valid(_) => ParserError(EOF(1), "No statements parsed").invalidNel
          case Invalid(errors) => Invalid(errors)
        }
      case Invalid(errors) =>
        fail(s"Scanner errors: ${errors.toList.mkString(", ")}")
    }
  }

  test("literal numbers") {
    val result = parseExpression("123")
    result match {
      case Valid(Literal(value)) => assertEquals(value.asInstanceOf[Double], 123.0)
      case other => fail(s"Expected literal number, got: $other")
    }
  }

  test("literal strings") {
    val result = parseExpression("\"hello\"")
    result match {
      case Valid(Literal(value)) => assertEquals(value, "hello")
      case other => fail(s"Expected literal string, got: $other")
    }
  }

  test("literal booleans") {
    val trueResult = parseExpression("true")
    trueResult match {
      case Valid(Literal(value)) => assertEquals(value, true)
      case other => fail(s"Expected literal true, got: $other")
    }

    val falseResult = parseExpression("false")
    falseResult match {
      case Valid(Literal(value)) => assertEquals(value, false)
      case other => fail(s"Expected literal false, got: $other")
    }
  }

  test("literal nil") {
    val result = parseExpression("nil")
    result match {
      case Valid(Literal(value)) => assertEquals(value, null)
      case other => fail(s"Expected literal nil, got: $other")
    }
  }

  test("grouping expressions") {
    val result = parseExpression("(123)")
    result match {
      case Valid(Grouping(Literal(value))) => assertEquals(value.asInstanceOf[Double], 123.0)
      case other => fail(s"Expected grouped literal, got: $other")
    }
  }

  test("unary expressions") {
    val minusResult = parseExpression("-123")
    minusResult match {
      case Valid(Unary(operator, Literal(value))) =>
        assert(operator.isInstanceOf[MINUS])
        assertEquals(value.asInstanceOf[Double], 123.0)
      case other => fail(s"Expected unary minus, got: $other")
    }

    val bangResult = parseExpression("!true")
    bangResult match {
      case Valid(Unary(operator, Literal(value))) =>
        assert(operator.isInstanceOf[BANG])
        assertEquals(value, true)
      case other => fail(s"Expected unary bang, got: $other")
    }
  }

  test("binary arithmetic expressions") {
    val addResult = parseExpression("1 + 2")
    addResult match {
      case Valid(Binary(Literal(left), operator, Literal(right))) =>
        assert(operator.isInstanceOf[PLUS])
        assertEquals(left.asInstanceOf[Double], 1.0)
        assertEquals(right.asInstanceOf[Double], 2.0)
      case other => fail(s"Expected binary addition, got: $other")
    }

    val mulResult = parseExpression("3 * 4")
    mulResult match {
      case Valid(Binary(Literal(left), operator, Literal(right))) =>
        assert(operator.isInstanceOf[STAR])
        assertEquals(left.asInstanceOf[Double], 3.0)
        assertEquals(right.asInstanceOf[Double], 4.0)
      case other => fail(s"Expected binary multiplication, got: $other")
    }
  }

  test("binary comparison expressions") {
    val gtResult = parseExpression("5 > 3")
    gtResult match {
      case Valid(Binary(Literal(left), operator, Literal(right))) =>
        assert(operator.isInstanceOf[GREATER])
        assertEquals(left.asInstanceOf[Double], 5.0)
        assertEquals(right.asInstanceOf[Double], 3.0)
      case other => fail(s"Expected greater than, got: $other")
    }

    val eqResult = parseExpression("1 == 1")
    eqResult match {
      case Valid(Binary(Literal(left), operator, Literal(right))) =>
        assert(operator.isInstanceOf[EQUAL_EQUAL])
        assertEquals(left.asInstanceOf[Double], 1.0)
        assertEquals(right.asInstanceOf[Double], 1.0)
      case other => fail(s"Expected equality, got: $other")
    }
  }

  test("operator precedence") {
    // 1 + 2 * 3 should parse as 1 + (2 * 3)
    val result = parseExpression("1 + 2 * 3")
    result match {
      case Valid(Binary(Literal(left), plus: PLUS, Binary(Literal(mulLeft), star: STAR, Literal(mulRight)))) =>
        assertEquals(left.asInstanceOf[Double], 1.0)
        assertEquals(mulLeft.asInstanceOf[Double], 2.0)
        assertEquals(mulRight.asInstanceOf[Double], 3.0)
      case other => fail(s"Expected correct precedence parsing, got: $other")
    }
  }

  test("left associativity") {
    // 1 - 2 - 3 should parse as (1 - 2) - 3
    val result = parseExpression("1 - 2 - 3")
    result match {
      case Valid(Binary(Binary(Literal(left), minus1: MINUS, Literal(middle)), minus2: MINUS, Literal(right))) =>
        assertEquals(left.asInstanceOf[Double], 1.0)
        assertEquals(middle.asInstanceOf[Double], 2.0)
        assertEquals(right.asInstanceOf[Double], 3.0)
      case other => fail(s"Expected left associativity, got: $other")
    }
  }

  test("complex nested expressions") {
    val result = parseExpression("(1 + 2) * (3 - 4)")
    result match {
      case Valid(Binary(
        Grouping(Binary(Literal(a), plus: PLUS, Literal(b))),
        star: STAR,
        Grouping(Binary(Literal(c), minus: MINUS, Literal(d)))
      )) =>
        assertEquals(a.asInstanceOf[Double], 1.0)
        assertEquals(b.asInstanceOf[Double], 2.0)
        assertEquals(c.asInstanceOf[Double], 3.0)
        assertEquals(d.asInstanceOf[Double], 4.0)
      case other => fail(s"Expected complex nested expression, got: $other")
    }
  }

  test("unary with higher precedence") {
    // -1 * 2 should parse as (-1) * 2
    val result = parseExpression("-1 * 2")
    result match {
      case Valid(Binary(Unary(minus: MINUS, Literal(left)), star: STAR, Literal(right))) =>
        assertEquals(left.asInstanceOf[Double], 1.0)
        assertEquals(right.asInstanceOf[Double], 2.0)
      case other => fail(s"Expected unary precedence, got: $other")
    }
  }

  test("string concatenation") {
    val result = parseExpression("\"hello\" + \"world\"")
    result match {
      case Valid(Binary(Literal(left), plus: PLUS, Literal(right))) =>
        assertEquals(left, "hello")
        assertEquals(right, "world")
      case other => fail(s"Expected string concatenation, got: $other")
    }
  }

  test("comparison chains") {
    val result = parseExpression("1 < 2 == true")
    result match {
      case Valid(Binary(Binary(Literal(a), less: LESS, Literal(b)), eq: EQUAL_EQUAL, Literal(c))) =>
        assertEquals(a.asInstanceOf[Double], 1.0)
        assertEquals(b.asInstanceOf[Double], 2.0)
        assertEquals(c, true)
      case other => fail(s"Expected comparison chain, got: $other")
    }
  }

  test("parse errors") {
    val result = parseExpression("1 +")
    result match {
      case Invalid(errors) =>
        assert(errors.toList.head.message.contains("Expect expression"))
      case Valid(expr) => fail(s"Expected parse error but got: $expr")
    }
  }

  test("mismatched parentheses") {
    val result = parseExpression("(1 + 2")
    result match {
      case Invalid(errors) =>
        assert(errors.toList.head.message.contains("Expect ')' after expression"))
      case Valid(expr) => fail(s"Expected parse error but got: $expr")
    }
  }

  test("multiple errors") {
    val result = parseExpression("1 + + 2")
    result match {
      case Invalid(errors) =>
        assert(errors.toList.head.message.contains("Expect expression"))
      case Valid(expr) => fail(s"Expected parse error but got: $expr")
    }
  }
}