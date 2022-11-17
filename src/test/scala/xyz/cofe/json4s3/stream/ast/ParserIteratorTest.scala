package xyz.cofe.json4s3.stream.ast

import xyz.cofe.json4s3.stream.token.TokenIterator
import AST._

class ParserIteratorTest extends munit.FunSuite:
  test("stream test") {
    assert(
      ParserIterator("1.5 [ 1, false ] true {} {a:null}").toList == 
      List(
        JsFloat(1.5),
        JsArray(List(JsInt(1), JsBool(false))),
        JsBool(true),
        JsObj(List()),
        JsObj(List(("a",JsNull))),
      )
    )
  }
