package xyz.cofe.json4s3.stream.ast

import xyz.cofe.json4s3.stream.ast.AST._
import xyz.cofe.json4s3.stream.ast._
import xyz.cofe.json4s3.stream.token.Tokenizer

class AST2JsonTest extends munit.FunSuite:
  test("ast2json") {
    val jsnString = JsObj(
      Map(
        "int" -> JsInt(1),
        "float" -> JsFloat(2.5),
        "big" -> JsBig(BigInt("123456789012345678901234567890")),
        "null" -> JsNull,
        "true" -> JsBool(true),
        "false" -> JsBool(false),
        "array" -> JsArray(List(
          JsInt(1), JsInt(2), JsInt(3),
        ))
      )
    ).json

    println(jsnString)

    val tokens = Tokenizer.parse(jsnString).getOrElse(List())    
    
    val jsTreeEt = Parser.parse(tokens)
    assert( jsTreeEt.isRight )

    val jsTree = jsTreeEt.map(_._1).getOrElse({
      throw new Error("!")
    })

    println(s"jsTree ${jsTree}")
    jsTree match
      case AST.JsObj(fields) =>
        assert(fields.contains("int"))
        assert(fields.contains("float"))
        assert(fields.contains("big"))
        assert(fields.contains("null"))
        assert(fields.contains("true"))
        assert(fields.contains("false"))
        assert(fields.contains("array"))
      case _ => fail("expect JsObj")    
  }

