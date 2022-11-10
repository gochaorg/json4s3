package xyz.cofe.jtfm.store.json.stream

/**
  * Токен
  */
enum Token:
  /** не распознанная лексема */
  case Undefined( val text:String ) extends Token

  /** Строка */
  case Str( val text:String ) extends Token

  /** Число */
  case Number( val text:String ) extends Token

  /** Квадратная скобка */
  case OpenSuqare( val text:String ) extends Token
  
  /** Квадратная скобка */
  case CloseSuqare( val text:String ) extends Token
  
  /** Фигурная скобка */
  case OpenBrace( val text:String ) extends Token
  
  /** Фигурная скобка */
  case CloseBrace( val text:String ) extends Token
  
  /** Запятая */
  case Comma( val text:String ) extends Token
  
  /** Двоеточие */
  case Colon( val text:String ) extends Token

  /** Пробел */
  case WhiteSpace( val text:String ) extends Token

  /** Идентификатор */
  case Identifier( val text:String ) extends Token

  /** Однострочный комментарий */
  case SLComment( val text:String ) extends Token

  /** Многострочный комментарий */
  case MLComment( val text:String ) extends Token
