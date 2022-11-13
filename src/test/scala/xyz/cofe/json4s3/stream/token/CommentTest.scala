package xyz.cofe.json4s3.stream.token

class CommentTest extends munit.FunSuite:
  test("// single") {
    val parser = comment.Parser()
    var state = parser.init

    state = parser.accept(state,'/')
    state = parser.accept(state,'/')
    state = parser.accept(state,'c')
    state = parser.end(state)
    assert( parser.ready(state) == Some(Token.SLComment("c")) )
  }

  test("/* multi") {
    val parser = comment.Parser()
    var state = parser.init

    state = parser.accept(state,'/')
    state = parser.accept(state,'*')
    state = parser.accept(state,'c')
    state = parser.accept(state,'*')
    state = parser.accept(state,'/')
    state = parser.end(state)
    assert( parser.ready(state) == Some(Token.MLComment("c")) )
  }
