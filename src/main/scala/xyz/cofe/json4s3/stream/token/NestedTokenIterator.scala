package xyz.cofe.json4s3.stream.token

import xyz.cofe.json4s3.stream.token.Token
import xyz.cofe.json4s3.errors.TokenIteratorClosed

class NestedTokenIterator( sourceIterator:Iterator[Token] ) extends Iterator[Token]:
  var closed = false
  var path = List[Token]()
  override def hasNext: Boolean = 
    if closed then
      false
    else
      sourceIterator.hasNext

  override def next(): Token = 
    if closed then
      throw TokenIteratorClosed()
    else
      val tok = sourceIterator.next()
      tok match
        case Token.Identifier(text) =>
          if path.isEmpty then closed = true
        case Token.Str(text) => 
          if path.isEmpty then closed = true
        case Token.IntNumber(num) =>
          if path.isEmpty then closed = true
        case Token.BigNumber(num) =>
          if path.isEmpty then closed = true
        case Token.FloatNumber(num) =>
          if path.isEmpty then closed = true
        case Token.OpenSquare =>
          path = tok :: path
        case Token.CloseSquare =>
          if path.isEmpty then
            closed = true
          else
            path = path.tail
            if path.isEmpty then
              closed = true
        case Token.OpenBrace =>
          path = tok :: path
        case Token.CloseBrace =>
          if path.isEmpty then
            closed = true
          else
            path = path.tail
            if path.isEmpty then
              closed = true
        case Token.Comma =>        
        case Token.Colon =>
        case Token.WhiteSpace(text) =>
        case Token.SLComment(text) =>
        case Token.MLComment(text) =>      
      tok