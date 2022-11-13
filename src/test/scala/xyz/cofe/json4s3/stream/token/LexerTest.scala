package xyz.cofe.json4s3.stream.token

class LexerTest extends munit.FunSuite:
  test("test") {
    val lexer = Tokenizer()
    var state = lexer.init
    val sample = "{}[],: 1 \"aaa\" /* abc */"
    // var tokens = List[Token]()
    // sample.map { chr => 
    //   println(s"chr '$chr' state=${state}")
    //   lexer.accept(state, chr) match
    //     case Left(err) => 
    //       throw new Error(err)
    //     case Right( (newState, toks) ) =>
    //       println(s"  newState=${newState} toks=${toks}")
    //       state = newState
    //       tokens = tokens ++ toks
    // }

    val tokensEt = sample.foldLeft( Right( (lexer.init, List()) ):Either[String,(Tokenizer.State,List[Token])] ){ case(sum,chr) => 
      sum.flatMap { case (state, tokens) =>
        lexer.accept(state,chr).map { case (newState, newTokens) => (newState,tokens ++ newTokens) }
      }
    }.map { _._2 }

    tokensEt.foreach(_.foreach(println))
  }