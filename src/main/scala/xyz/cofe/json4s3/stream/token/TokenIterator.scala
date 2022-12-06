package xyz.cofe.json4s3.stream.token

import xyz.cofe.json4s3.errors._

import java.io.Reader
import scala.util.Try
import scala.util.Failure
import scala.util.Success
import java.io.StringReader

object TokenIterator:
  def apply(reader:Reader):TokenIterator =
    new TokenIterator(
      Tokenizer.State.Init,
      reader
    )
  def apply(string:String):TokenIterator =
    apply(new StringReader(string))

/** Итератор по лексемам */
class TokenIterator(
  private var state: Tokenizer.State,
  private val reader: Reader,
) extends Iterator[Token]:
  private var error: Option[TokenIteratorError] = None
  private var buffer: List[Token] = List.empty
  private var closed = false
  def isClosed = closed
  private var tokenizerEnd = false

  private def fetch(state:Tokenizer.State, reader:Reader):(Tokenizer.State, List[Token]) =
    if closed 
      then (state, List.empty)
      else
        if tokenizerEnd then 
          closed = true
          (state, List.empty)
        else
          var stop = false
          var curState = state
          var toks : List[Token] = List()
          while !stop do
            Try( reader.read() ) match
              case Failure(exception) => throw new TokenIteratorIOError(exception)
              case Success(charCode) => charCode match
                case -1 =>
                  stop = true
                  tokenizerEnd = true
                  Tokenizer.end(curState) match
                    case Left(err) => throw new TokenIteratorTokenizer(err)
                    case Right((newState,tokens)) =>
                      curState = newState
                      if tokens.nonEmpty then
                        toks = tokens
                      else
                        closed = true
                case _ =>
                  Tokenizer.accept(curState, charCode.toChar) match
                    case Left(err) =>  throw new TokenIteratorTokenizer(err)
                    case Right((newState,tokens)) => 
                      curState = newState
                      if tokens.nonEmpty then
                        toks = tokens
                        stop = true
          (curState, toks)

  try
    val (st, toks) = fetch(state, reader)
    state = st
    buffer = toks
  catch
    case e:TokenIteratorError =>
      error = Some(e)

  override def hasNext: Boolean = closed match
    case true => false
    case false => error match
      case Some(err) => throw err
      case None => buffer.headOption match
        case Some(value) => true
        case None =>
          try
            val (st, toks) = fetch(state, reader)
            if closed then
              false
            else
              state = st
              buffer = toks
              true
          catch
            case e:TokenIteratorError =>
              error = Some(e)
              throw e
    
  override def next(): Token = closed match
    case true => throw new TokenIteratorClosed()
    case false => buffer.headOption match
      case Some(value) => 
        buffer = buffer.tail
        value
      case None => 
        try
          val (st, toks) = fetch(state, reader)
          if closed then
            throw new TokenIteratorClosed()
          else
            state = st
            buffer = toks.tail
            toks.head
        catch
          case e:TokenIteratorError =>
            error = Some(e)
            throw e
    
  
