package com.rokim.lox

import cats.data.Validated.Invalid
import cats.data.Validated.Valid
import cats.data.ValidatedNec

import java.io.{BufferedReader, InputStreamReader}
import java.nio.charset.Charset
import java.nio.file.{Files, Paths}
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
      case Valid(tokens) => println(tokens)
      case Invalid(e) => println(s"Scanner errors: ${e.toList.mkString}")
    }
  }

}
