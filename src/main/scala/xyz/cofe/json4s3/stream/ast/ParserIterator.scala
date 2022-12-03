package xyz.cofe.json4s3.stream.ast

import xyz.cofe.json4s3.stream.token.TokenIterator
import xyz.cofe.json4s3.stream.token.Token
import xyz.cofe.json4s3.errors._
import java.io.Reader

object ParserIterator:
  def apply(tokens: Iterator[Token]):ParserIterator =
    new ParserIterator(Parser.State.Init, tokens)

  def apply(reader: Reader):ParserIterator =
    new ParserIterator(
      Parser.State.Init,
      TokenIterator(reader)
    )

  def apply(string: String):ParserIterator =
    new ParserIterator(
      Parser.State.Init,
      TokenIterator(string)
    )

/** Итератор по лексемам, генерирует на выходе Json объекты */
class ParserIterator(
  private var state:Parser.State,
  private var tokenIterator:Iterator[Token]
) extends Iterator[AST]:
  private var fetched: Option[AST] = None
  private var error: Option[ParserIteratorError] = None

  private def fetch():Option[AST] =
    var res:Option[AST] = None
    try 
      var stop = false
      while !stop do
        if !tokenIterator.hasNext then
          stop = true
        else
          val tok = tokenIterator.next()
          Parser.accept(state, tok) match
            case Left(err) => throw ParserIteratorParserError(err)
            case Right((newState,Some(ast))) =>
              state = newState
              res = Some(ast)
              stop = true
            case Right((newState,None)) =>
              state = newState
      res
    catch
      case terr:TokenIteratorClosed =>
        throw ParserIteratorClosed()
      case err:TokenIteratorError =>
        throw ParserIteratorTokError(err)

  try
    fetched = fetch()
  catch
    case e:ParserIteratorError =>
      error = Some(e)
  override def hasNext: Boolean = 
    error match
      case Some(err) =>
        throw err
      case None => fetched match
        case Some(value) =>
          true
        case None => 
          false
  
  override def next(): AST = 
    error match
      case Some(err) => throw err
      case None => fetched match
        case None => throw ParserIteratorClosed()
        case Some(value) =>
          try
            fetched = fetch()
          catch
            case e:ParserIteratorError => throw e
          value
