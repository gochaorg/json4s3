package xyz.cofe.json4s3.stream.token

class TokenIteratorTest extends munit.FunSuite:
  test("read tokens") {
    assert(TokenIterator("123 true false").toList == List(
      Token.IntNumber(123),
      Token.WhiteSpace(" "),
      Token.Identifier("true"),
      Token.WhiteSpace(" "),
      Token.Identifier("false"),
    ))
  }
