package xyz.cofe.json4s3.stream.ast

import java.util.concurrent.ThreadLocalRandom
import xyz.cofe.json4s3.stream.ast.AST._
import xyz.cofe.json4s3.stream.ast.AST
import xyz.cofe.json4s3.stream.ast.Parser
import xyz.cofe.rnd

class ParserRandomTest extends munit.FunSuite:

  test("try parse") {
    val astJs = astRnd.js()
    (0 until 50).foreach { _=> 
      Parser.parseSeq( astJs.tokens ) match
        case Left(err) => fail(err.toString())
        case Right(value) => ()
    }
  }