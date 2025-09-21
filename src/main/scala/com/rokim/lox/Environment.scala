package com.rokim.lox

import scala.collection.mutable

object Environment {
  def apply(): Environment = new Environment(None)
}
class Environment private (val enclosing: Option[Environment]) {

  val values = mutable.Map.empty[String, Any]

  def define(name: IDENTIFIER, value: Any): Unit =
    values(name.lexeme) = value

  def get(name: IDENTIFIER): Option[Any] =
    values
      .get(name.lexeme)
      .orElse(enclosing.flatMap(_.get(name)))

  // True if the var is defined. Else False
  def assign(name: Token, value: Any): Boolean = {
    values.get(name.lexeme) match {
      case Some(_) =>
        values.update(name.lexeme, value)
        true
      case None => enclosing.exists(_.assign(name, value))
    }
  }

  def childEnv(): Environment = new Environment(Some(this))
}
