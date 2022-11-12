package xyz.cofe.json4s3.stream.token

/**
  * Токен / Лексема JSON
  */
enum Token:
  /** не распознанная лексема */
  case Undefined( val text:String ) extends Token

  /** Строка - литерал */
  case Str( val text:String ) extends Token

  /** Число */
  case IntNumber( val num:Int ) extends Token

  /** Число */
  case BigNumber( val num:BigInt ) extends Token

  /** Число */
  case FloatNumber( val num:Double ) extends Token

  /** Квадратная скобка */
  case OpenSuqare extends Token
  
  /** Квадратная скобка */
  case CloseSuqare extends Token
  
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
