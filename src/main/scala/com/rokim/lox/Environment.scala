package com.rokim.lox

import scala.collection.mutable

class Environment {
  val values = mutable.Map.empty[String, Any]

  def define(name: IDENTIFIER, value: Any): Unit = {
    values(name.lexeme) = value
  }

  def get(name: IDENTIFIER): Option[Any] = {
    values.get(name.lexeme)
  }
  
  // True if the var is defined. Else False
  def assign(name: Token, value: Any): Boolean = {
    values.get(name.lexeme) match {
      case Some(_) => values.update(name.lexeme, value)
        true
      case None => false
    }
  }
}
