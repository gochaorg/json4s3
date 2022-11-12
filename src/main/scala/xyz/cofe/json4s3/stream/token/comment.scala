package xyz.cofe.json4s3.stream.token

/** 
 * комментарий js
 * 
 * comment             ::= single-line-comment | multi-line-comment
 * single-line-comment ::= '/' '/' {any} ( end-of-line | end-of-input )
 * end-of-line         ::= '\r' '\n' | '\n'
 * multi-line-comment  ::= '/' '*' {any} ( '*' '/' | end-of-input )
 */
object comment:
  enum State extends StreamTokenParserState:
    // '/' -> SelectSLML
    // -> Err
    case Init extends State

    // '/' -> ExpectEOL
    // '*' -> ExpectML0
    // -> Err
    case SelectSLML extends State

    // '\r' -> ExpectN
    // '\n' -> FinishConsumed
    // -> ExpectEOL
    case ExpectEOL( buffer:StringBuilder ) extends State

    // '\n' -> FinishConsumed
    // -> Err
    case ExpectN( buffer:StringBuilder ) extends State

    // '*' -> ExpectML1
    // -> ExpectML0
    case ExpectML0( buffer:StringBuilder ) extends State

    // '/' -> FinishConsumed
    // -> ExpectML0
    case ExpectML1( buffer:StringBuilder ) extends State
    
    case Err extends State
    case Finish( buffer:StringBuilder, singleLine:Boolean ) extends State
    case FinishConsumed( buffer:StringBuilder, singleLine:Boolean ) extends State

    override def isAcceptable: Boolean = this match
      case State.Err => false
      case _:State.Finish => false
      case _:State.FinishConsumed => false
      case _ => true
    
    override def isConsumed: Boolean = this match
      case _:State.FinishConsumed => true
      case _ => false
    
    override def isReady: Boolean = this match
      case _:State.ExpectEOL => true
      case _:State.ExpectN => true
      case _:State.ExpectML0 => true
      case _:State.ExpectML1 => true
      case _:State.Finish => true
      case _:State.FinishConsumed => true
      case _ => false
    
    override def isError: Boolean = this match
      case State.Err => true
      case _ => false    

  class Parser() extends StreamTokenParser[Char]:
    override type OUT = Token
    override type STATE = State

    override def init: State = State.Init
    override def ready(state: State): Option[Token] = state match
      case State.Init => None
      case State.SelectSLML => None
      case State.ExpectEOL(buffer) =>
        Some(Token.SLComment(buffer.toString()))
      case State.ExpectN(buffer) =>
        Some(Token.SLComment(buffer.toString()))
      case State.ExpectML0(buffer) =>
        Some(Token.MLComment(buffer.toString()))
      case State.ExpectML1(buffer) =>
        Some(Token.MLComment(buffer.toString()))
      case State.Err => None
      case State.Finish(buffer,singleLine) =>
        Some(if singleLine then
          Token.SLComment(buffer.toString())
        else
          Token.MLComment(buffer.toString()))
      case State.FinishConsumed(buffer,singleLine) =>
        Some(if singleLine then
          Token.SLComment(buffer.toString())
        else
          Token.MLComment(buffer.toString()))
    
    override def tail(state: State): Option[Token] = ready(state)

    override def accept(state: State, char: Char): State = state match
      case State.Init => char match
        case '/' => State.SelectSLML
        case _ => State.Err
      case State.SelectSLML => char match
        case '/' => State.ExpectEOL(new StringBuilder)
        case '*' => State.ExpectML0(new StringBuilder)
        case _ => State.Err
      case s@State.ExpectEOL(buffer) => char match
        case '\r' => 
          buffer.append(char)
          State.ExpectN(buffer)
        case '\n' => 
          buffer.append(char)
          State.FinishConsumed(buffer,singleLine = true)
        case _ => 
          buffer.append(char)
          s
      case State.ExpectN(buffer) => char match
        case '\n' => 
          buffer.append(char)
          State.FinishConsumed(buffer,singleLine = true)
        case _ => State.Err
      case s@State.ExpectML0(buffer) => char match
        case '*' => 
          State.ExpectML1(buffer)
        case _ => 
          buffer.append(char)
          s
      case State.ExpectML1(buffer) => char match
        case '/' =>
           State.FinishConsumed(buffer,singleLine = false)
        case _ => 
          buffer.append('*')
          buffer.append(char)
          State.ExpectML0(buffer)
      case State.Err => State.Err
      case s:State.Finish => s
      case s:State.FinishConsumed => s
    
    override def end(state: State): State = state match
      case State.Init => State.Err
      case State.SelectSLML => State.Err
      case State.ExpectEOL(buffer) => State.Finish(buffer,singleLine = true)
      case State.ExpectN(buffer) => State.Finish(buffer,singleLine = true)
      case State.ExpectML0(buffer) => State.Finish(buffer,singleLine = false)
      case State.ExpectML1(buffer) => State.Finish(buffer,singleLine = false)
      case State.Err => State.Err
      case s:State.Finish => s
      case s:State.FinishConsumed => s
    

