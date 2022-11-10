package xyz.cofe.jtfm.store.json.stream

/** Распознание пробельных символов */
object whitespace {
  enum State extends StreamTokenParserState:
    case Init extends State
    case Err  extends State
    case Work(buffer:StringBuilder) extends State
    case Finish(str:String) extends State 

    override def isError: Boolean = this match
      case State.Init => false
      case State.Err => true
      case State.Work(buffer) => false
      case State.Finish(str) => false
    
    override def isReady: Boolean = this match
      case State.Init => false
      case State.Err => false
      case State.Work(buffer) => true
      case State.Finish(str) => true
    override def isAcceptable: Boolean = this match
      case State.Init => true
      case State.Err => false
      case State.Work(buffer) => true
      case State.Finish(str) => false

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
              State.Err
        case State.Err => State.Err
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
        case State.Err => None
        case State.Work(buffer) => Some(Token.WhiteSpace(buffer.toString))
        case State.Finish(str) => Some(Token.WhiteSpace(str))
      

    override def tail(state: State): Option[Token.WhiteSpace] = 
      state match
        case State.Init => None
        case State.Err => None
        case State.Work(buffer) => Some(Token.WhiteSpace(buffer.toString))
        case State.Finish(str) => Some(Token.WhiteSpace(str))

    override def end(state: State): State =
      state match
        case State.Init => State.Err
        case State.Err => State.Err
        case State.Work(buffer) => State.Finish(buffer.toString())
        case s:State.Finish => s
      
}
