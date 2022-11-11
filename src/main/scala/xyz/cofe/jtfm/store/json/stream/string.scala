package xyz.cofe.jtfm.store.json.stream

/**
  * Парсинг строки
  * https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Grammar_and_types
  * 
  * 
  * string ::= singe_quoted_string | double_quoted_string
  * singe_quoted_string  ::= '\'' { encoded_char } '\''
  * double_quoted_string ::= '"' { encoded_char } '"'
  * encoded_char ::= escaped_seq | simple_char
  * escaped_seq ::= escape_hex | escape_unicode_ext | escape_unicode | escape_oct | escape_simple
  * escape_oct ::= '\' oct_char oct_char oct_char
  * escape_simple ::= '\' ( '0' | 'b' | 'f' | 'n' | 'r' | 't' | 'v' | '\'' | '"' | '\' )
  * escape_hex ::= '\x' hex_char hex_char
  * escape_unicode ::= '\u' hex_char hex_char hex_char hex_char
  * escape_unicode_ext ::= '\u{' hex_char hex_char hex_char hex_char hex_char '}'
  */
object string:
  enum State extends StreamTokenParserState:
    // Init - ' -> SimpleChar
    // Init - " -> SimpleChar
    // Init -> Err
    case Init extends State
    case Err extends State

    // SimpleChar - quoteChar -> Finish
    // SimpleChar - \         -> EscStart
    // SimpleChar             -> SimpleChar    
    case SimpleChar( quoteChar:Char, decoded:StringBuilder ) extends State

    // EscStart - x -> EscHex
    // EscStart - u -> EscUnicodeStart
    // EscStart - 0 -> EscZero
    // EscStart - 1..7 -> EscOct
    // EscStart - b | f | n | r | t | v | \ | ' | " -> SimpleChar
    // EscStart -> Err
    case EscStart( quoteChar:Char, decoded:StringBuilder ) extends State

    // EscHex - 0..9 a..f A..F hexDigit.size = 0 -> EscHex
    // EscHex - 0..9 a..f A..F hexDigit.size = 1 -> SimpleChar
    // EscHex -> Err
    case EscHex( quoteChar:Char, decoded:StringBuilder, hexDigit:List[Int] ) extends State

    // EscUnicodeStart - 0..9 a..f A..F -> EscUnicode4digit
    // EscUnicodeStart - '{' -> EscUnicode5digit
    // EscUnicodeStart -> Err
    case EscUnicodeStart( quoteChar:Char, decoded:StringBuilder ) extends State

    // EscUnicode4digit - 0..9 a..f A..F hexDigit.size in (1,2,3) -> EscUnicode4digit
    // EscUnicode4digit - 0..9 a..f A..F hexDigit.size in (4)     -> SimpleChar
    // EscUnicode4digit -> Err
    case EscUnicode4digit( quoteChar:Char, decoded:StringBuilder, hexDigits:List[Int] ) extends State

    // EscUnicode5digit - 0..9 a..f A..F hexDigit.size in (1,2,3,4) -> EscUnicode5digit
    // EscUnicode5digit - '}' -> SimpleChar
    // EscUnicode5digit -> Err
    case EscUnicode5digit( quoteChar:Char, decoded:StringBuilder, hexDigits:List[Int] ) extends State

    // EscZero - 0..7 -> EscOct
    // EscZero - quoteChar -> Finish
    // EscZero - \ -> EscStart
    // EscZero -   -> SimpleChar
    case EscZero( quoteChar:Char, decoded:StringBuilder ) extends State

    // EscOct - 0..7 octDigit.size in (0,1) -> EscOct
    // EscOct - 0..7 octDigit.size in (2) -> SimpleChar
    // EscOct -> Err
    case EscOct( quoteChar:Char, decoded:StringBuilder, octDigit:List[Char] ) extends State

    case Finish( decoded:String ) extends State

    override def isError: Boolean = this match
      case State.Err => true
      case _ => false

    override def isReady: Boolean = this match
      case State.Finish(decoded) => true
      case _ => false

    override def isAcceptable: Boolean = this match
      case State.Err => false
      case _:State.Finish => false
      case _ => true
    
  class Parser extends StreamTokenParser[Char]:
    override type STATE = State
    override type OUT = Token.Str

    override def init: State = ???

    override def ready(state: State): Option[Token.Str] = state match
      case State.Finish(decoded) => Some(Token.Str(decoded))
      case _ => None
    
    override def tail(state: State): Option[Token.Str] = None

    private def hex(chr:Char):Option[Int] = chr match
      case '0' => Some(0)
      case '1' => Some(1)
      case '2' => Some(2)
      case '3' => Some(3)
      case '4' => Some(4)
      case '5' => Some(5)
      case '6' => Some(6)
      case '7' => Some(7)
      case '8' => Some(8)
      case '9' => Some(9)
      case 'a' | 'A' => Some(10)
      case 'b' | 'B' => Some(11)
      case 'c' | 'C' => Some(12)
      case 'd' | 'D' => Some(13)
      case 'e' | 'E' => Some(14)
      case 'f' | 'F' => Some(15)

    override def accept(state: State, char: Char): State = state match
      case State.Init => char match
        case '\'' => State.SimpleChar(char, new StringBuilder() )
        case '"' => State.SimpleChar(char, new StringBuilder() )
        case _ => State.Err
      case State.Err =>  State.Err
      case s@State.SimpleChar(quoteChar, decoded) => char match
        case _ if quoteChar==char => State.Finish(decoded.toString())
        case '\\' => State.EscStart(quoteChar, decoded)
        case _ => decoded.append(char); s
      case State.EscStart(quoteChar, decoded) => char match
        case 'x' => State.EscHex(quoteChar, decoded, List())
        case 'u' => State.EscUnicodeStart(quoteChar, decoded)
        case '0' => State.EscZero(quoteChar, decoded)
        case '1'|'2'|'3'|'4'|'5'|'6'|'7' => State.EscOct(quoteChar,decoded,List(char))
        case 'b'|'f'|'n'|'r'|'t'|'v'|'"'|'\\'|'\'' =>
          char match
            case 'b' => decoded.append("\u0008")
            case 'f' => decoded.append("\u000c")
            case 'n' => decoded.append("\u000a")
            case 'r' => decoded.append("\u000d")
            case 't' => decoded.append("\u0009")
            case 'v' => decoded.append("\u000b")
            case '"' => decoded.append("\"")
            case '\'' => decoded.append("\'")
            case _ =>
          State.SimpleChar(quoteChar,decoded)
        case _ => State.Err
      case State.EscHex(quoteChar, decoded, hexDigit) => 
        val digit = hex(char)
        char match
          case _ if digit.isDefined && hexDigit.size == 0 => State.EscHex(quoteChar,decoded,List(digit.get))
          case _ if digit.isDefined && hexDigit.size == 1 => 
            val digits = hexDigit :+ digit.get
            val chrFromDigit = ((digits(0) << 4) | (digits(1)))
            java.lang.Character.toString(chrFromDigit).foreach { c => decoded.append(c) }
            State.SimpleChar(quoteChar, decoded)
      case State.EscUnicodeStart(quoteChar, decoded) => 
        val digit = hex(char)
        char match
          case '{' => State.EscUnicode5digit(quoteChar,decoded,List())
          case _ if digit.isDefined =>
            State.EscUnicode4digit(quoteChar,decoded,List(digit.get))
          case _ => State.Err
      case State.EscUnicode4digit(quoteChar,decoded,hexDigits) =>
        val digit = hex(char)
        if hexDigits.size < 3 && digit.isDefined then
          State.EscUnicode4digit(quoteChar, decoded, hexDigits :+ digit.get)
        else if hexDigits.size == 3 && digit.isDefined then
          val digits = hexDigits :+ digit.get
          val chrFromDigit = (digits(0) << 12) | (digits(1) << 8) | (digits(2) << 4) | (digits(3))
          java.lang.Character.toString(chrFromDigit).foreach { c => decoded.append(c) }
          State.SimpleChar(quoteChar, decoded)
        else
          State.Err
      case State.EscUnicode5digit(quoteChar,decoded,hexDigits) =>
        val digit = hex(char)
        if hexDigits.size < 5 && digit.isDefined then
          State.EscUnicode5digit(quoteChar, decoded, hexDigits :+ digit.get)
        else if hexDigits.size == 5 && char=='}' then
          val digits = hexDigits :+ digit.get
          val chrFromDigit = (digits(0) << 16) |(digits(1) << 12) | (digits(2) << 8) | (digits(3) << 4) | (digits(4))
          java.lang.Character.toString(chrFromDigit).foreach { c => decoded.append(c) }
          State.SimpleChar(quoteChar, decoded)
        else
          State.Err
      case State.EscZero(quoteChar, decoded) => char match
        case '0'|'1'|'2'|'3'|'4'|'5'|'6'|'7' => 
          State.EscOct(quoteChar,decoded,List(char))
        case _ if quoteChar==char => 
          decoded.append("\u0000")
          State.Finish(decoded.toString())
        case '\\' =>
          decoded.append("\u0000")
          State.EscStart(quoteChar,decoded)
        case _ =>
          decoded.append("\u0000")
          State.SimpleChar(quoteChar, decoded)
      case State.EscOct(quoteChar, decoded, octDigit) => char match
        case '0'|'1'|'2'|'3'|'4'|'5'|'6'|'7' if octDigit.size < 2 =>
          State.EscOct(quoteChar,decoded,octDigit :+ char)
        case '0'|'1'|'2'|'3'|'4'|'5'|'6'|'7' if octDigit.size == 2 =>
          val digits = (octDigit :+ char).map(c => hex(c).get)
          val num = digits(0)*8*8 + digits(0)*8 + digits(0)
          java.lang.Character.toString(num).foreach( c => decoded.append(c) )
          State.SimpleChar(quoteChar,decoded)
        case _ => State.Err
      case s:State.Finish => s
    
    override def end(state: State): State = state match
      case _ => State.Err
    
