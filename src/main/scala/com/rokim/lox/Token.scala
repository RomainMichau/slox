package com.rokim.lox

sealed trait Token {
  def lexeme: String
  def line: Int
}

sealed trait UnaryOperatorToken  extends Token
sealed trait BinaryOperatorToken extends Token

// Single-character tokens
case class LEFT_PAREN(line: Int)  extends Token                                       { val lexeme = "(" }
case class RIGHT_PAREN(line: Int) extends Token                                       { val lexeme = ")" }
case class LEFT_BRACE(line: Int)  extends Token                                       { val lexeme = "{" }
case class RIGHT_BRACE(line: Int) extends Token                                       { val lexeme = "}" }
case class COMMA(line: Int)       extends Token                                       { val lexeme = "," }
case class DOT(line: Int)         extends Token                                       { val lexeme = "." }
case class MINUS(line: Int)       extends BinaryOperatorToken with UnaryOperatorToken { val lexeme = "-" }
case class PLUS(line: Int)        extends BinaryOperatorToken                         { val lexeme = "+" }
case class SEMICOLON(line: Int)   extends Token                                       { val lexeme = ";" }
case class SLASH(line: Int)       extends BinaryOperatorToken                         { val lexeme = "/" }
case class STAR(line: Int)        extends BinaryOperatorToken                         { val lexeme = "*" }

// One or two character tokens
case class BANG(line: Int)          extends UnaryOperatorToken  { val lexeme = "!"  }
case class BANG_EQUAL(line: Int)    extends BinaryOperatorToken { val lexeme = "!=" }
case class EQUAL(line: Int)         extends Token               { val lexeme = "="  }
case class EQUAL_EQUAL(line: Int)   extends BinaryOperatorToken { val lexeme = "==" }
case class GREATER(line: Int)       extends BinaryOperatorToken { val lexeme = ">"  }
case class GREATER_EQUAL(line: Int) extends BinaryOperatorToken { val lexeme = ">=" }
case class LESS(line: Int)          extends BinaryOperatorToken { val lexeme = "<"  }
case class LESS_EQUAL(line: Int)    extends BinaryOperatorToken { val lexeme = "<=" }

// Literals
case class IDENTIFIER(lexeme: String, line: Int)              extends Token
case class STRING(lexeme: String, line: Int, literal: String) extends Token
case class NUMBER(lexeme: String, line: Int, literal: Double) extends Token

// Keywords
case class AND(line: Int)       extends Token { val lexeme = "and"    }
case class CLASS(line: Int)     extends Token { val lexeme = "class"  }
case class ELSE(line: Int)      extends Token { val lexeme = "else"   }
case class FALSE(line: Int)     extends Token { val lexeme = "false"  }
case class FUN(line: Int)       extends Token { val lexeme = "fun"    }
case class FOR(line: Int)       extends Token { val lexeme = "for"    }
case class IF(line: Int)        extends Token { val lexeme = "if"     }
case class NIL(line: Int)       extends Token { val lexeme = "nil"    }
case class OR(line: Int)        extends Token { val lexeme = "or"     }
case class PRINT_TKN(line: Int) extends Token { val lexeme = "print"  }
case class RETURN(line: Int)    extends Token { val lexeme = "return" }
case class SUPER(line: Int)     extends Token { val lexeme = "super"  }
case class THIS(line: Int)      extends Token { val lexeme = "this"   }
case class TRUE(line: Int)      extends Token { val lexeme = "true"   }
case class VAR_TKN(line: Int)   extends Token { val lexeme = "var"    }
case class WHILE(line: Int)     extends Token { val lexeme = "while"  }

case class EOF(line: Int) extends Token { val lexeme = "" }
