package xyz.cofe.json4s3.stream.token

/**
  * Токен / Лексема JSON
  */
enum Token:
  /** Строка - литерал */
  case Str( val text:String ) extends Token

  /** Число */
  case IntNumber( val num:Int ) extends Token

  /** Число */
  case BigNumber( val num:BigInt ) extends Token

  /** Число */
  case FloatNumber( val num:Double ) extends Token

  /** Квадратная скобка */
  case OpenSquare extends Token
  
  /** Квадратная скобка */
  case CloseSquare extends Token
  
  /** Фигурная скобка */
  case OpenBrace extends Token
  
  /** Фигурная скобка */
  case CloseBrace extends Token
  
  /** Запятая */
  case Comma extends Token
  
  /** Двоеточие */
  case Colon extends Token

  /** Пробел */
  case WhiteSpace( val text:String ) extends Token

  /** Идентификатор, например null / false / some... */
  case Identifier( val text:String ) extends Token

  /** Однострочный комментарий */
  case SLComment( val text:String ) extends Token

  /** Многострочный комментарий */
  case MLComment( val text:String ) extends Token

  def json:String = this match
    case Token.Str(text) => {
      val sb = new StringBuilder
      sb.append("\"")
      text.foreach { chr => 
        val ichr = chr.toInt
        ichr match
          case 0x08 => sb.append("\\b")
          case 0x0c => sb.append("\\f")
          case 0x0a => sb.append("\\n")
          case 0x0d => sb.append("\\r")
          case 0x09 => sb.append("\\t")
          case 0x0b => sb.append("\\v")
          case _ => chr match
            case '"'  => sb.append("\\\"")
            case '\'' => sb.append("'")
            case _ =>
              if java.lang.Character.isLetterOrDigit(chr) then
                sb.append(chr)
              else if java.lang.Character.isWhitespace(chr) then
                sb.append(chr)
              else if ichr>=32 && ichr<256 then
                sb.append(chr)
              else
                val hex = ichr.toHexString
                hex.length match
                  case 5 => sb.append("\\u{").append(hex).append("}")
                  case 4 => sb.append("\\u").append(hex)
                  case 3 => sb.append("\\u0").append(hex)
                  case 2 => sb.append("\\u00").append(hex)
                  case 1 => sb.append("\\u000").append(hex)
                  case _ => sb.append(chr)
      }
      sb.append("\"")
      sb.toString()
    }

    case Token.IntNumber(num) => num.toString()
    case Token.BigNumber(num) => num.toString()+"n"
    case Token.FloatNumber(num) => num.toString()
    case Token.OpenSquare => "["
    case Token.CloseSquare => "]"
    case Token.OpenBrace => "{"
    case Token.CloseBrace => "}"
    case Token.Comma => ","
    case Token.Colon => ":"
    case Token.WhiteSpace(text) => text
    case Token.Identifier(text) => text
    case Token.SLComment(text) => "//" + text
    case Token.MLComment(text) => "/*" + text + "*/"
    
