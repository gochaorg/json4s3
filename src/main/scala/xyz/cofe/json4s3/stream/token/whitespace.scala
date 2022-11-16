package xyz.cofe.json4s3.stream.token

import xyz.cofe.json4s3.errors._
import xyz.cofe.json4s3.errors.TokenError._

/** Распознание пробельных символов */
object whitespace {
  enum State extends StreamTokenParserState:
    case Init extends State
    case Err( err:TokenError )  extends State
    case Work(buffer:StringBuilder) extends State
    case Finish(str:String) extends State 

    override def isError: Boolean = this match
      case State.Init => false
      case _:State.Err => true
      case State.Work(buffer) => false
      case State.Finish(str) => false
    
    override def isReady: Boolean = this match
      case State.Init => false
      case _:State.Err => false
      case State.Work(buffer) => true
      case State.Finish(str) => true
    override def isAcceptable: Boolean = this match
      case State.Init => true
      case _:State.Err => false
      case State.Work(buffer) => true
      case State.Finish(str) => false
    override def isConsumed: Boolean = this match
      case State.Init => false
      case _:State.Err => false
      case State.Work(buffer) => true
      case State.Finish(str) => false
    override def error: Option[TokenError] = this match
      case State.Err(err) => Some(err)
      case _ => None    
    

  class Parser extends StreamTokenParser[Char]:
    override type STATE = State
    override type OUT = Token.WhiteSpace

    override def init: State = State.Init

    def isWs(c:Char):Boolean = Character.isWhitespace(c.asInstanceOf[Character])

    override def accept(state: State, char: Char): State =
      state match
        case State.Init => 
          isWs(char) match
            case true =>
              val sb = new StringBuilder
              sb.append(char)
              State.Work(sb)
            case false =>
              State.Err(notMatchInput(this,state,char,"whitespace char"))
        case s:State.Err => s
        case s@State.Work(buffer) =>
          isWs(char) match
            case true =>
              buffer.append(char)
              s
            case false =>
              State.Finish(buffer.toString())
        case s:State.Finish => s

    override def ready(state: State): Option[Token.WhiteSpace] = 
      state match
        case State.Init => None
        case _:State.Err => None
        case State.Work(buffer) => Some(Token.WhiteSpace(buffer.toString))
        case State.Finish(str) => Some(Token.WhiteSpace(str))
      

    override def tail(state: State): Option[Token.WhiteSpace] = 
      state match
        case State.Init => None
        case _:State.Err => None
        case State.Work(buffer) => Some(Token.WhiteSpace(buffer.toString))
        case State.Finish(str) => Some(Token.WhiteSpace(str))

    override def end(state: State): State =
      state match
        case State.Init => State.Err(NoInput())
        case s:State.Err => s
        case State.Work(buffer) => State.Finish(buffer.toString())
        case s:State.Finish => s
      
}
