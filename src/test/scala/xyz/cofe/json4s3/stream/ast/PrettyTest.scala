package xyz.cofe.json4s3.stream.ast

import xyz.cofe.json4s3.stream.token.Tokenizer

class PrettyTest extends munit.FunSuite:
  test("pretty 1") {
    println("="*40)
    println("pretty 1")

    val jsTreeEt = Parser.parse(
      Tokenizer.parse(
        """
        {
          e: [[]]
        }
        """
      ).getOrElse(List())
    )
    
    val jsTree = 
      jsTreeEt.map(_._1).getOrElse { throw new Error("parse fail") }

    println(jsTree.json)

    Parser.parse(Tokenizer.parse(jsTree.json).getOrElse {
      throw new Error()
    }) match
      case Left(err) => fail(err)
      case Right(value) =>    
  }

  test("pretty 2") {
    println("="*40)
    println("pretty 2")

    val jsTreeEt = Parser.parse(
      Tokenizer.parse(
        """
        { a: 'abc'
        , b: {}
        , c: [1,2,3]
        , d: 
          { a: true
          , b: false
          , c: []
          }
        , e: [[]]
        }
        """
      ).getOrElse(List())
    )
    
    val jsTree = 
      jsTreeEt.map(_._1).getOrElse { throw new Error("parse fail") }

    implicit val fmt = FormattingJson
      .pretty(true)
      // .afterColon("")
      // .commaSpace("")

    val json = jsTree.json
    println(json)

    Parser.parse(Tokenizer.parse(json).getOrElse {
      throw new Error()
    }) match
      case Left(err) => fail(err)
      case Right(value) =>    
  }
