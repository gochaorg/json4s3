package xyz.cofe.json4s3.stream.token

import scala.collection.mutable
import xyz.cofe.json4s3.errors._
import xyz.cofe.json4s3.errors.TokenError._


/** распознание идентификатора */
object identifier:
  enum State extends StreamTokenParserState:
    case Init extends State
    case Err( err:TokenError ) extends State
    case Work(buffer:StringBuilder) extends State
    case Finish(string:String) extends State

    override def error: Option[TokenError] = this match
      case State.Err(err) => Some(err)
      case _ => None    
      
    override def isAcceptable: Boolean = this match
      case State.Init => true
      case _:State.Err => false
      case _:State.Work => true
      case _:State.Finish => false
    
    override def isReady: Boolean = this match
      case State.Init => false
      case _:State.Err => false
      case _:State.Work => true
      case _:State.Finish => true
    override def isError: Boolean = this match
      case State.Init => false
      case _:State.Err => true
      case _:State.Work => false
      case _:State.Finish => false
    override def isConsumed: Boolean = this match
      case State.Init => false
      case _:State.Err => false
      case State.Work(buffer) => true
      case State.Finish(string) => false
    

  class Parser extends StreamTokenParser[Char]:
    override def init: State = State.Init

    override def accept(state: State, char: Char): State = state match
      case State.Init => 
        (Character.isLetter(char2Character(char)) || char=='_' || char=='$') match
          case true =>
            val sb = mutable.StringBuilder()
            sb.append(char)
            State.Work(sb)
          case false =>
            State.Err(notMatchInput(this,state,char,"letter or _ or $"))
      
      case s:State.Err => s
      case s@State.Work(buffer) =>
        (  Character.isLetter( char2Character(char) ) 
        || Character.isDigit( char2Character(char) ) 
        || char=='_'
        || char=='$'
        ) match
          case true =>
            buffer.append(char)
            s
          case false =>
            State.Finish(buffer.toString())
      case s:State.Finish => s
    
    override def end(state: State): State = state match
      case State.Init => State.Err(NoInput())
      case s:State.Err => s
      case State.Work(buffer) => State.Finish(buffer.toString())
      case s:State.Finish => s
    

    override def ready(state: State): Option[Token.Identifier] = state match
      case State.Init => None
      case _:State.Err => None
      case State.Work(buffer) => Some(Token.Identifier(buffer.toString()))
      case State.Finish(string) => Some(Token.Identifier(string))
    
    override def tail(state: State): Option[Token.Identifier] = state match
      case State.Init => None
      case _:State.Err => None
      case State.Work(buffer) => Some(Token.Identifier(buffer.toString()))
      case State.Finish(string) => Some(Token.Identifier(string))

    override type STATE = State
    override type OUT = Token.Identifier
    
