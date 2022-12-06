package xyz.cofe.json4s3.stream.token

import xyz.cofe.json4s3.errors.TokenError
import scala.annotation.newMain

//import xyz.cofe.json4s3.stream.token.Tok

class TokenizerTest extends munit.FunSuite:
  test("test") {
    //val lexer = new Tokenizer()
    //var state = lexer.init
    val sample = "{}[],: 1 \"aaa\" /* abc */"
    val tokensEt = sample.foldLeft( Right( (Tokenizer.State.Init, List()) ):Either[TokenError,(Tokenizer.State,List[Token])] ){ case(sum,chr) => 
      sum.flatMap { case (state, tokens) =>
        Tokenizer.accept(state,chr).map { case (newState, newTokens) => (newState,tokens ++ newTokens) }
      }
    }.map { _._2 }

    tokensEt.foreach(_.foreach(println))
  }

  test("sample") {
    import xyz.cofe.json4s3.stream.token.Tokenizer.{accept => tokenize}
    import xyz.cofe.json4s3.stream.token.Token
    import xyz.cofe.json4s3.stream.token.Tokenizer.State

    val parse0Et = tokenize(
      State.Init, // Начальное состояние
      '{'         // Входной символ
    )
    assert(parse0Et.isRight) 

    parse0Et.foreach { case(newState,tokens) => 
      assert(tokens == List(Token.OpenBrace)) // Распознаная лексема
      tokenize(newState,'}').foreach { case(newState,tokens) => 
        assert(tokens == List(Token.CloseBrace)) // Распознаная лексема
      }
    }
  }

  test("multiple") {
    import xyz.cofe.json4s3.stream.token.Tokenizer.{accept => tokenize}
    import xyz.cofe.json4s3.stream.token.Token
    import xyz.cofe.json4s3.stream.token.Tokenizer.State

    // Начальное состояние
    var parserState = State.Init

    // Завршение цикла обработки строки
    var stop = false

    // Входная строка
    var chars = "{} []:, 12 true 'string'".toList

    // Обработка входной строки
    while !chars.isEmpty && !stop do
      tokenize(parserState,chars.head) match
        case Left(err) => 
          // Ошибка входной последовательности
          println(s"catch error $err")
          stop = true
        case Right((newState,tokens)) =>
          // Очередное состояние парсера
          println(s"input char '${chars.head}' input.state=${parserState}")          
          println(s"  output state  = ${newState}")
          println(s"  output tokens = ${tokens}")

          // меняем состояние парсера
          parserState = newState

          // откидываем обработанный символ 
          chars = chars.tail
  }

  test("parse string") {
    import xyz.cofe.json4s3.stream.token.Tokenizer
    Tokenizer.parse("12 true") match
      case Left(err) => 
        println(err)
      case Right(tokenList) =>
        tokenList.foreach(println)
  }