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
  enum State:
    // Init - ' -> SimpleChar
    // Init - " -> SimpleChar
    // Init -> Err
    case Init
    case Err

    // SimpleChar - quoteChar -> Finish
    // SimpleChar - \         -> EscStart
    // SimpleChar             -> SimpleChar    
    case SimpleChar( quoteChar:Char, text:StringBuilder, decoded:StringBuilder )

    // EscStart - x -> EscHex
    // EscStart - u -> EscUnicodeStart
    // EscStart - 0 -> EscZero
    // EscStart - 1..7 -> EscOct
    // EscStart - b | f | n | r | t | v | \ | ' | " -> SimpleChar
    // EscStart -> Err
    case EscStart( quoteChar:Char, text:StringBuilder, decoded:StringBuilder )

    // EscHex - 0..9 a..f A..F hexDigit.size = 0 -> EscHex
    // EscHex - 0..9 a..f A..F hexDigit.size = 1 -> SimpleChar
    // EscHex -> Err
    case EscHex( quoteChar:Char, text:StringBuilder, decoded:StringBuilder, hexDigit:List[Char] )

    // EscUnicodeStart - 0..9 a..f A..F -> EscUnicode4digit
    // EscUnicodeStart - '{' -> EscUnicode5digit
    // EscUnicodeStart -> Err
    case EscUnicodeStart( quoteChar:Char, text:StringBuilder, decoded:StringBuilder )

    // EscUnicode4digit - 0..9 a..f A..F hexDigit.size in (1,2,3) -> EscUnicode4digit
    // EscUnicode4digit - 0..9 a..f A..F hexDigit.size in (4)     -> SimpleChar
    // EscUnicode4digit -> Err
    case EscUnicode4digit( quoteChar:Char, text:StringBuilder, hexDigit:List[Char] )

    // EscUnicode5digit - 0..9 a..f A..F hexDigit.size in (1,2,3,4) -> EscUnicode5digit
    // EscUnicode5digit - '}' -> SimpleChar
    // EscUnicode5digit -> Err
    case EscUnicode5digit( quoteChar:Char, text:StringBuilder, decoded:StringBuilder, hexDigit:List[Char] )

    // EscZero - 1..7 -> EscOct
    // EscZero -> SimpleChar
    case EscZero( quoteChar:Char, text:StringBuilder, decoded:StringBuilder )

    // EscOct - 1..7 octDigit.size in (0,1) -> EscOct
    // EscOct - 1..7 octDigit.size in (2) -> SimpleChar
    // EscOct -> Err
    case EscOct( quoteChar:Char, text:StringBuilder, decoded:StringBuilder, octDigit:List[Char] )

    case Finish( text:String, decoded:String )
