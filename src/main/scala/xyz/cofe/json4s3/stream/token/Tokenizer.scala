package xyz.cofe.json4s3.stream.token

import xyz.cofe.json4s3.errors._
import xyz.cofe.json4s3.errors.TokenError._

object Tokenizer:
  enum State:
    // '-' -> NumParse
    // '.' -> NumParse
    // '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> NumParse
    // '{' | '}' | '[' | ']' | ',' | ':' -> OneCharParse
    // '\'' | '"' -> StrParse
    // whitespace -> WhitespaceParse
    // '/' -> CommentParse
    // letter | '$' | '_' -> IdParser
    // -> Err
    case Init
    case NumParse( parser:number.Parser, state:number.State )
    case StrParse( parser:string.Parser, state:string.State )
    case IdParser( parser:identifier.Parser, state:identifier.State )
    case CommentParse( parser:comment.Parser, state:comment.State )
    case WhitespaceParse( parser:whitespace.Parser, state:whitespace.State )

  def parse(str:String) = {
    val inst = new Tokenizer()
    inst.parse(str)
  }

/**
  * Парсинг лексем Json
  */
class Tokenizer:
  import Tokenizer._

  extension [P <: StreamTokenParserState](state:P)
    def succFinish:Boolean = {
      !state.isError && !state.isAcceptable && state.isReady
    }

  /**
    * Парсинг строки
    *
    * @param string строка
    * @return лексеммы
    */
  def parse(string:String):Either[TokenError,List[Token]] = {
    string.foldLeft( Right( (init, List()) ):Either[TokenError,(State,List[Token])] ){ case(sum,chr) => 
      sum.flatMap { case (state, tokens) =>
        accept(state,chr).map { case (newState, newTokens) => (newState,tokens ++ newTokens) }
      }
    }.flatMap { case( (state,tokens) ) => 
      end(state).flatMap { case (newState, newTokens) => 
        Right( 
          tokens ++ newTokens
        )
      }
    }
  }

  def init:State = State.Init

  def accept(state:State, char:Char):Either[TokenError,(State,List[Token])] = state match
    case State.Init => char match
      case '-' | '.' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
        val parser = number.Parser()
        val st = parser.accept(parser.init, char)
        Right( (State.NumParse(parser, st), List()) )
      case '{' => Right((State.Init, List(Token.OpenBrace)))
      case '}' => Right((State.Init, List(Token.CloseBrace)))
      case '[' => Right((State.Init, List(Token.OpenSquare)))
      case ']' => Right((State.Init, List(Token.CloseSquare)))
      case ',' => Right((State.Init, List(Token.Comma)))
      case ':' => Right((State.Init, List(Token.Colon)))
      case '"' | '\'' => 
        val parser = string.Parser()
        val st = parser.accept(parser.init, char)
        Right( (State.StrParse(parser, st), List()) )
      case _ if Character.isWhitespace(char) =>
        val parser = whitespace.Parser()
        val st = parser.accept(parser.init, char)
        Right((State.WhitespaceParse(parser, st),List()))
      case '/' =>
        val parser = comment.Parser()
        val st = parser.accept(parser.init, char)
        Right((State.CommentParse(parser, st),List()))
      case _ if Character.isLetter(char) || "_$".contains(char) =>
        val parser = identifier.Parser()
        val st = parser.accept(parser.init, char)
        Right(( State.IdParser(parser, st),List()))
      case _ =>
        Left(DidNotMatchExpectInput("expect: - . 0..9 { } [ ] , : / whitespace letters _ $"))

    //////////////////////////////////////////////////////////////////////////////
    case State.NumParse(parser, state) =>
      val newState = parser.accept(state, char)
      if newState.isError then
        Left(DidNotMatchExpectInput(s"can't parse number char '$char', number parser state: $newState"))
      else if newState.succFinish then
        if newState.isConsumed then
          Right(( State.Init , List(parser.ready(newState).get )) )
        else
          val tok0 = parser.ready(newState).get
          val next = accept( State.Init, char )
          next.map { case (s,toks) => (s, tok0 :: toks) }
      else
        Right( State.NumParse(parser,newState), List() )

    //////////////////////////////////////////////////////////////////////////////
    case State.StrParse(parser, state) =>
      val newState = parser.accept(state, char)
      if newState.isError then
        Left(DidNotMatchExpectInput(s"can't parse number char '$char', number parser state: $newState"))
      else if newState.succFinish then
        if newState.isConsumed then
          Right(( State.Init , List(parser.ready(newState).get )) )
        else
          val tok0 = parser.ready(newState).get
          val next = accept( State.Init, char )
          next.map { case (s,toks) => (s, tok0 :: toks) }
      else
        Right( State.StrParse(parser,newState), List() )

    //////////////////////////////////////////////////////////////////////////////
    case State.IdParser(parser, state) =>
      val newState = parser.accept(state, char)
      if newState.isError then
        Left(DidNotMatchExpectInput(s"can't parse number char '$char', number parser state: $newState"))
      else if newState.succFinish then
        if newState.isConsumed then
          Right(( State.Init , List(parser.ready(newState).get )) )
        else
          val tok0 = parser.ready(newState).get
          val next = accept( State.Init, char )
          next.map { case (s,toks) => (s, tok0 :: toks) }
      else
        Right( State.IdParser(parser,newState), List() )

    //////////////////////////////////////////////////////////////////////////////
    case State.CommentParse(parser, state) =>
      val newState = parser.accept(state, char)
      if newState.isError then
        Left(DidNotMatchExpectInput(s"can't parse number char '$char', number parser state: $newState"))
      else if newState.succFinish then
        if newState.isConsumed then
          Right(( State.Init , List(parser.ready(newState).get )) )
        else
          val tok0 = parser.ready(newState).get
          val next = accept( State.Init, char )
          next.map { case (s,toks) => (s, tok0 :: toks) }
      else
        Right( State.CommentParse(parser,newState), List() )

    //////////////////////////////////////////////////////////////////////////////
    case State.WhitespaceParse(parser, state) =>
      val newState = parser.accept(state, char)
      if newState.isError then
        Left(DidNotMatchExpectInput(s"can't parse number char '$char', number parser state: $newState"))
      else if newState.succFinish then
        if newState.isConsumed then
          Right(( State.Init , List(parser.ready(newState).get )) )
        else
          val tok0 = parser.ready(newState).get
          val next = accept( State.Init, char )
          next.map { case (s,toks) => (s, tok0 :: toks) }
      else
        Right( State.WhitespaceParse(parser,newState), List() )

  def end(state:State):Either[TokenError,(State,List[Token])] = state match
    case State.Init => Right((State.Init,List()))
    case State.NumParse(parser, state) =>
      val newState = parser.end(state)
      if newState.succFinish then
        val tok0 = parser.ready(newState).get
        Right( (State.Init, List(tok0)) )
      else
        Right( (State.Init, List()) )
    case State.StrParse(parser, state) =>
      val newState = parser.end(state)
      if newState.succFinish then
        val tok0 = parser.ready(newState).get
        Right( (State.Init, List(tok0)) )
      else
        Right( (State.Init, List()) )
    case State.IdParser(parser, state) =>
      val newState = parser.end(state)
      if newState.succFinish then
        val tok0 = parser.ready(newState).get
        Right( (State.Init, List(tok0)) )
      else
        Right( (State.Init, List()) )
    case State.CommentParse(parser, state) =>
      val newState = parser.end(state)
      if newState.succFinish then
        val tok0 = parser.ready(newState).get
        Right( (State.Init, List(tok0)) )
      else
        Right( (State.Init, List()) )
    case State.WhitespaceParse(parser, state) =>
      val newState = parser.end(state)
      if newState.succFinish then
        val tok0 = parser.ready(newState).get
        Right( (State.Init, List(tok0)) )
      else
        Right( (State.Init, List()) )
  