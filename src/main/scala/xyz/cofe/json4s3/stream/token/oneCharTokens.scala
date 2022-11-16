package xyz.cofe.json4s3.stream.token

import xyz.cofe.json4s3.errors._
import xyz.cofe.json4s3.errors.TokenError._

/** Распознование односимвольных лексем */
object oneCharTokens:
  /** состояние распознователя/парсера */
  enum State extends StreamTokenParserState:
    case Init extends State
    case Err( err:TokenError ) extends State
    case Finish(tok:Token) extends State
    override def isAcceptable: Boolean = this match
      case State.Init => true
      case _:State.Err => false
      case _:State.Finish => false
    
    override def isReady: Boolean = this match
      case State.Init => false
      case _:State.Err => false
      case _:State.Finish => true
    override def isError: Boolean = this match
      case _:State.Err => true
      case _ => false
    override def isConsumed: Boolean = this match
      case State.Init => false
      case _:State.Err => false
      case _:State.Finish => true

    override def error: Option[TokenError] = this match
      case State.Err(err) => Some(err)
      case _ => None    

  class Parser extends StreamTokenParser[Char]:
    override type STATE = State
    override type OUT = Token

    override def init: State = State.Init

    override def accept(state: State, char: Char): State = state match
      case State.Init => char match
        case '{' => State.Finish(Token.OpenBrace)
        case '}' => State.Finish(Token.CloseBrace)
        case '[' => State.Finish(Token.OpenSuqare)
        case ']' => State.Finish(Token.CloseSuqare)
        case ',' => State.Finish(Token.Comma)
        case ':' => State.Finish(Token.Colon)
        case _ => State.Err(notMatchInput(this,state,char,"{ } [ ] , ;"))
      case s:State.Err => s
      case s:State.Finish => s
    
    override def end(state: State): State = state match
      case State.Init => State.Err(NoInput())
      case s:State.Err => s
      case s:State.Finish => s
    
    override def ready(state: State): Option[Token] = state match
      case State.Init => None
      case _:State.Err => None
      case State.Finish(tok) => Some(tok)
    
    override def tail(state: State): Option[Token] = state match
      case State.Init => None
      case _:State.Err => None
      case State.Finish(tok) => Some(tok)
