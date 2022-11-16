package xyz.cofe.json4s3.stream.token

import xyz.cofe.json4s3.errors.TokenError

//import xyz.cofe.json4s3.stream.token.Tok

class TokenizerTest extends munit.FunSuite:
  test("test") {
    val lexer = new Tokenizer()
    var state = lexer.init
    val sample = "{}[],: 1 \"aaa\" /* abc */"
    val tokensEt = sample.foldLeft( Right( (lexer.init, List()) ):Either[TokenError,(Tokenizer.State,List[Token])] ){ case(sum,chr) => 
      sum.flatMap { case (state, tokens) =>
        lexer.accept(state,chr).map { case (newState, newTokens) => (newState,tokens ++ newTokens) }
      }
    }.map { _._2 }

    tokensEt.foreach(_.foreach(println))
  }