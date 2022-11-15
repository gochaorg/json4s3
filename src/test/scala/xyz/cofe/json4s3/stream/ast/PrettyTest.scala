package xyz.cofe.json4s3.stream.ast

import xyz.cofe.json4s3.stream.token.Tokenizer

class PrettyTest extends munit.FunSuite:
  test("aa") {
    val jsTreeEt = Parser.parse(
      Tokenizer.parse(
        """
        { a: 'abc'
        , b: {}
        , c: [1,2,3]
        , d: 
          { a: true
          , b: false
          }
        }
        """
      ).getOrElse(List())
    )
    
    val jsTree = 
      jsTreeEt.map(_._1).getOrElse { throw new Error("parse fail") }

    println(jsTree.json)
  }
