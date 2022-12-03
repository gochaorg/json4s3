package xyz.cofe.json4s3.stream.token

import Token._

class NestedTokenIteratorTest extends munit.FunSuite:
  test("nested token test") {
    val jsonString = 
      """|{
         |  "hits" : [
         |    { "a": 1 },
         |    { "b": 2 },
         |    { "c": 3 },
         |    { "d": 4 }   
         |   ]
         |}
      """.stripMargin.replace("\n","").replaceAll(" +","")

    val srcTIter = TokenIterator(jsonString)

    // skip: { "hits" : [
    (0 until 4).foreach { _ => srcTIter.next() }

    var nestedIter = new NestedTokenIterator(srcTIter)
    assert( nestedIter.toList == List( OpenBrace, Str("a"), Colon, IntNumber(1), CloseBrace ) )

    // expect: ,
    assert( srcTIter.next() == Comma )
    nestedIter = new NestedTokenIterator(srcTIter)
    assert( nestedIter.toList == List( OpenBrace, Str("b"), Colon, IntNumber(2), CloseBrace ) )

    // expect: ,
    assert( srcTIter.next() == Comma )
    nestedIter = new NestedTokenIterator(srcTIter)
    assert( nestedIter.toList == List( OpenBrace, Str("c"), Colon, IntNumber(3), CloseBrace ) )

    // expect: ,
    assert( srcTIter.next() == Comma )
    nestedIter = new NestedTokenIterator(srcTIter)
    assert( nestedIter.toList == List( OpenBrace, Str("d"), Colon, IntNumber(4), CloseBrace ) )
  }