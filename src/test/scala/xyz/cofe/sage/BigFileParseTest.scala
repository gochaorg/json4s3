package xyz.cofe.sage

import java.nio.file.Path
import java.nio.file.Files
import xyz.cofe.json4s3.stream.token.TokenIterator
import xyz.cofe.json4s3.stream.ast.ParserIterator
import xyz.cofe.json4s3.stream.ast.AST

class BigFileParseTest extends munit.FunSuite:
  // val smallFile = Path.of("/Users/g.kamnev/code/_alerts/incidents/2022-11-19--02/small-set.json")
  // test("small file tokens") {
  //   if Files.exists(smallFile) then
  //     println("small file exists")
  //     val reader = Files.newBufferedReader(smallFile)
  //     val tokenIterator = TokenIterator(reader)
  //     println( tokenIterator.toList.size )
  // }

  // test("small file nested objects") {
  //   if Files.exists(smallFile) then
  //     println("small file exists")
  //     val reader = Files.newBufferedReader(smallFile)
  //     val tokenIterator = TokenIterator(reader)
  //     (0 until 4).foreach( _ => tokenIterator.next() )

  //     val parserIter = ParserIterator(tokenIterator)
  //     val jsValue = parserIter.next()
  //     // skip first 4 tokens
  //     // tokenIterator.take(4)
  //     // val parserIter = ParserIterator(tokenIterator)
  //     jsValue match
  //       case AST.JsStr(value) => 
  //       case AST.JsFloat(value) =>
  //       case AST.JsInt(value) =>
  //       case AST.JsBig(value) =>
  //       case AST.JsNull =>
  //       case AST.JsBool(value) =>
  //       case AST.JsArray(value) =>
  //       case obj@AST.JsObj(value) =>
  //         println(obj)
  //         println(value.size)    
  //     println( tokenIterator.next() )
  // }

  test("sub json parse") {
    val jsonString = 
      """|{
         |  "hits" : [
         |    { "a": 1 },
         |    { "a": 2 },
         |    { "a": 3 },
         |    { "a": 4 }   
         |   ]
         |}
      """.stripMargin.replace("\n","").replaceAll(" +","")
    TokenIterator(jsonString).toList.foreach(println)

    val tokenIterator = TokenIterator(jsonString)
    (0 until 4).foreach { _ => tokenIterator.next() }
    
    val parserIterator = ParserIterator(tokenIterator)
    println(parserIterator.next())
  }