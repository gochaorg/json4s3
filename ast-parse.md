Парсинг AST
================

- _Какие есть узлы AST_
- _Парсинг AST_
- _Итератор_
- _Грамматика JSON_

Какие есть узлы AST
----------------------

```scala
enum AST:
  case JsStr( value:String )
  case JsFloat( value:Double )
  case JsInt( value:Int )
  case JsBig( value:BigInt )
  case JsNull
  case JsBool( value:Boolean )
  case JsArray( value:Seq[AST] )
  case JsObj( value:List[(String,AST)] )
```

Парсинг AST
----------------------

Из `String` 

```scala
val astEt : Either[ParserError,AST] = 
  Parser.parse(
    """
    { "a": 1,
      "b": {
        "c": 2
      }
    }
    """)
```

Из `Iterator[Token]`

```scala
val astEt : Either[ParserError,AST] = 
  Parser.parse( TokenIterator("{ sameple: 'json' }") )
```

Пасрер содержит состояние которое, представлено такой структурой

```scala
enum State:
  case Init 
  case ArrExpectValue( value:List[AST], parent:Option[State] )
  case ArrExpectComma( value:List[AST], parent:Option[State] )
  case ArrAfterComma( value:List[AST], parent:Option[State] )
  case ObjExpFieldName( value:List[(String,AST)], parent:Option[State] )
  case ObjAfterFieldName( fieldName:String, value:List[(String,AST)], parent:Option[State] )
  case ObjExpFieldValue( fieldName:String, value:List[(String,AST)], parent:Option[State] )
  case ObjExpectComma( value:List[(String,AST)], parent:Option[State] )
  case ObjAfterComma( value:List[(String,AST)], parent:Option[State] )
```

Данная структура основана на грамматике JSON.
При поступлении очередной лексемы, происходит смена состояния с одного на другое.
Грамматика описана ниже.

Для последовательного парсинга, по одной лексеме за раз есть такой метод

```scala
object Parser:
  /**
   * Парсинг по одной лексеме за один раз
   * @param state текущее состояние парсера
   * @param token лексема
   * @return Или ошибка - не соответствие грамматике
   *         Или новое состояние + возможно распознанный элемент Json
   */
  def accept(state:State, token:Token):Either[ParserError,(State,Option[AST])]
```


Пример с использованием состония парсера и последовательностью лексем

```scala
assert( 
  Parser.accept(Parser.State.Init, Token.Str("str123")) == Right(
    Parser.State.Init,
    Some(AST.JsStr("str123"))
  )
)
```

```scala
val result = Tokenizer
.parse("{'a':1,'b':2}")
.getOrElse(List())
.foldLeft( Right((Parser.State.Init,None)):Either[String,(Parser.State,Option[AST])] ){ case (sum,tok) => 
  sum.flatMap { case (state, _) => 
    val res = Parser.accept(state,tok)
    println(s"parse $tok => $res")
    res.left.map(_.toString())
  }
}
```

```scala
val result = List(
  Token.OpenSquare,
  Token.CloseSquare,
).foldLeft( Right((Parser.State.Init,None)):Either[String,(Parser.State,Option[AST])] ){ case (sum,tok) => 
  sum.flatMap { case (state, _) => 
    val res = Parser.accept(state,tok)
    println(s"parse $tok => $res")
    res.left.map(_.toString())
  }
}

assert(result.isRight)

val (state,resultJsOpt) = result.getOrElse( (Parser.State.Init, None) )
assert(resultJsOpt.isDefined)

assert( resultJsOpt.get == AST.JsArray(List()) )
```

Итератор
----------------------

```scala
assert(
  ParserIterator("1.5 [ 1, false ] true {} {a:null}").toList == 
  List(
    JsFloat(1.5),
    JsArray(List(JsInt(1), JsBool(false))),
    JsBool(true),
    JsObj(List()),
    JsObj(List(("a",JsNull))),
  )
)
```

Грамматика JSON
----------------------

Грамматика основа на лексеммах описанных в секции [tokenizer](tokenizer.md)

Ниже для справки какие есть лексеммы

  - `Str( val text:String )` - представляет строку
  - `IntNumber( val num:Int )` - представляет число - целое
  - `BigNumber( val num:BigInt )` - представляет число - большое целое
  - `FloatNumber( val num:Double )` - представляет число - плавующее
  - `OpenSquare` - квадратная скобка [
  - `CloseSquare` - квадратная скобка ]
  - `OpenBrace` - фигурная скобка {
  - `CloseBrace` - фигурная скобка }
  - `Comma` - запятая ,
  - `Colon` - двоеточие :
  - `WhiteSpace( val text:String )` - пробелный символ
  - `Identifier( val text:String )` - идентификатор - имеется виду true | false | null
  - `SLComment( val text:String )` - однострочный коментарий
  - `MLComment( val text:String )` - многострочный коментарий

....

    grammar ::= expression { expression }   
    expression ::= { skipToken } ( string | number | null | bool | array | object )   
    skipToken ::= SLComment | MLComment | WhiteSpace    
    string ::= JsStr
    number ::= JsFloat | JsInt | JsBig
    JsStr ::= Str
    JsFloat ::= FloatNumber
    JsInt   ::= IntNumber
    JsBig   ::= BigNumber
    null ::= Identifier(null)
    bool ::= Identifier(true) | Identifier(false)
    array ::= OpenSuqare expression { Comma expression } [ Comma ] CloseSuqare
            | OpenSuqare [ Comma ] CloseSuqare
    object ::= OpenBrace fieldKeyValue { Comma fieldKeyValue } [ Comma ] CloseSuqare
              | OpenBrace [ Comma ] CloseSuqare
    fieldKeyValue ::= fieldName {skipToken} Colon expression

Согласно синтаксису допустакются следующие конструкции

    { "a" : "value" }
    {  a  : 'value' }

Обе эти конструкции являются равно значными, 
так же допускаются комментарии

    { // single line comment
      /* multi line comment */
    }