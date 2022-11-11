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

  /** Идентификатор */
  case Identifier( val text:String ) extends Token

  /** Однострочный комментарий */
  case SLComment( val text:String ) extends Token

  /** Многострочный комментарий */
  case MLComment( val text:String ) extends Token
