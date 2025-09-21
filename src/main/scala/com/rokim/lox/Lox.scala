package com.rokim.lox

import cats.data.NonEmptyList
import cats.data.Validated.{Invalid, Valid}
import com.rokim.lox.Interpreter.InterpreterRuntimeError
import com.rokim.lox.Parser.ParserError

import scala.io.{Source, StdIn}
import scala.util.{Try, Using}


case class LoxError(line: Int, where: String, message: String) {
  println(s"[line $line] Error $where : $message")
}

object Lox {
  def main(args: Array[String]): Unit = {
    args.toList match {
      case path :: Nil => runFile(path)
      case _ => runPrompt()
    }
  }

  def runFile(path: String): Try[Unit] = {
    Using(Source.fromFile(path)) { source =>
      run(source.mkString)
    }
  }

  def runPrompt(): Unit = {
    @annotation.tailrec
    def loop(): Unit = {
      print("> ")
      Option(StdIn.readLine()) match {
        case Some(line) =>
          run(line)
          loop()
        case None => // EOF reached, exit gracefully
      }
    }
    loop()
  }


  def run(source: String): Unit = {
    Scanner.scan(source) match {
      case Valid(tokens) =>
        println(s"Tokens: ${tokens.mkString(" ")}")
        new Parser(tokens).parse() match {
        case Valid(stmts) =>
          Interpreter.interprete(stmts) match {
            case Valid(r) =>
            case Invalid(r) => printRuntimeErr(r)
          }
        case Invalid(e) => printParserErr(e)
      }
      case Invalid(e) => printScannerErr(e)
    }
  }

  private def printScannerErr(err: NonEmptyList[LoxError]): Unit = {
    err.toList.foreach { error =>
      System.err.println(s"[line ${error.line}] Error ${error.where}: ${error.message}")
    }
  }

  private def printParserErr(err: NonEmptyList[ParserError]): Unit = {
    err.toList.foreach { error =>
      System.err.println(s"Parser error: ${error.message} at line ${error.token.line}")
    }
  }

  private def printRuntimeErr(err: NonEmptyList[InterpreterRuntimeError]): Unit = {
    err.toList.foreach { error =>
      System.err.println(error.message +
        "\n[line " + error.token.line + "]")
    };
  }

}
