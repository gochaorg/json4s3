package xyz.cofe.json4s3.desc

import xyz.cofe.json4s3.stream.ast.Parser
import JsType._

class JsonDescribeTest extends munit.FunSuite:
  test("desc") {
    val desc =
      List(
        """{ "a": 1, "b":2 }""",
        """{ "a": 123, "c":"abc" }""",
        """{ "a": true, "d":[1,2], "b": { "x": 1 } }""",
      ).map( Parser.parse )
      .map( _.toOption )
      .flatten
      .foldLeft( Prod(Map.empty):JsType ){ case (jsType,ast) => jsType.merge(JsonDescribe.describe(ast)) }
    
    println(desc)
    assert( desc == Prod(Map(
        "a" -> Sum(Map(
          JsInt -> 2,
          JsBool -> 1
        )),
        "b" -> Sum(Map(
          JsInt -> 1,
          Prod(Map("x" -> JsInt)) -> 1
        )),
        "c" -> JsStr,
        "d" -> JsArray,
      )
    ))
  }