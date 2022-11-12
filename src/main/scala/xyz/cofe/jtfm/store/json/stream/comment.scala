package xyz.cofe.jtfm.store.json.stream

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
    case ExpectEOL extends State

    // '\n' -> FinishConsumed
    // -> Err
    case ExpectN extends State

    // '*' -> ExpectML1
    // -> ExpectML0
    case ExpectML0 extends State

    // '/' -> FinishConsumed
    // -> ExpectML0
    case ExpectML1 extends State
    
    case Err extends State
    case Finish extends State
    case FinishConsumed extends State

    override def isAcceptable: Boolean = this match
      case State.Err => false
      case State.Finish => false
      case State.FinishConsumed => false
      case _ => true
    
    override def isConsumed: Boolean = this match
      case State.FinishConsumed => true
      case _ => false
    
    override def isReady: Boolean = this match
      case State.ExpectEOL => true
      case State.ExpectN => true
      case State.ExpectML0 => true
      case State.ExpectML1 => true
      case State.Finish => true
      case State.FinishConsumed => true
      case _ => false
    
    override def isError: Boolean = this match
      case State.Err => true
      case _ => false    
