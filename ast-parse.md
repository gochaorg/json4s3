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

Пасрер содержит состояние которое представлено такой структурой

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

Грамматика JSON
----------------------

