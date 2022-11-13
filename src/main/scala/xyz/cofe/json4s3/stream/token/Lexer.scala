package xyz.cofe.json4s3.stream.token

class Lexer:
  enum State:
    // '-' -> NumParse
    // '.' -> NumParse
    // '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> NumParse
    // '{' | '}' | '[' | ']' | ',' | ':' -> OneCharParse
    // '\'' | '"' -> StrParse
    // whitespace -> WhitespaceParse
    // '/' -> CommentParse
    // letter | '$' | '_' -> IdParser
    // -> Err
    case Init
    case NumParse( parser:number.Parser, state:number.State )
    case StrParse( parser:string.Parser, state:string.State )
    case IdParser( parser:identifier.Parser, state:identifier.State )
    case OneCharParse
    case CommentParse( parser:comment.Parser, state:comment.State )
    case WhitespaceParse( parser:whitespace.Parser, state:whitespace.State )
    case Err( from:State )

  
