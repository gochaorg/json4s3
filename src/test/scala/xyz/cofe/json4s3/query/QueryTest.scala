package xyz.cofe.json4s3.query

import xyz.cofe.json4s3.query
import xyz.cofe.json4s3.stream.ast.Parser

class QueryTest extends munit.FunSuite:
  test("sample") {
    val astEt = Parser.parse(
      """{
           a: 1,
           b: {
            c: 2
           }
         }
      """)
    
    assert( astEt.query("a").int == Right(1) )
    assert( astEt.query("b")("c").int == Right(2) )
  }

