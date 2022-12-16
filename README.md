json4s3
===========

json4s3 - Это библиотека для парсинга json

Lexem parse (tokenizer)
--------------------------

[Основная статья о лексическом анализе](tokenizer.md)

- _Какие есть лексемы_
- _Парсер лексем_
- _Итератор по лексемам_
- _Вложенный итератор по лексемам_

### Короткие примеры

```scala
import xyz.cofe.json4s3.stream.token.Tokenizer
Tokenizer.parse("12 true") match
  case Left(err) => 
    println(err)
  case Right(tokenList) =>
    tokenList.foreach(println)
```

Вывод

    IntNumber(12)
    WhiteSpace( )
    Identifier(true)

```scala
assert(TokenIterator("123 true false").toList == List(
  Token.IntNumber(123),
  Token.WhiteSpace(" "),
  Token.Identifier("true"),
  Token.WhiteSpace(" "),
  Token.Identifier("false"),
))
```

### Какие есть лексемы

`enum Token:`

- `case Str( val text:String )` - представляет строку
- `case IntNumber( val num:Int )` - представляет число - целое
- `case BigNumber( val num:BigInt )` - представляет число - большое целое
- `case FloatNumber( val num:Double )` - представляет число - плавующее
- `case OpenSquare` - квадратная скобка `[`
- `case CloseSquare` - квадратная скобка `]`
- `case OpenBrace` - фигурная скобка `{`
- `case CloseBrace` - фигурная скобка `}`
- `case Comma` - запятая `,`
- `case Colon` - двоеточие `:`
- `case WhiteSpace( val text:String )` - пробелный символ
- `case Identifier( val text:String )` - идентификатор - имеется виду `true` | `false` | `null`
- `case SLComment( val text:String )` - однострочный коментарий 
- `case MLComment( val text:String )` - многострочный коментарий


Ast parse
-------------------

[Основная статья о парсинге](ast-parse.md)

- _Какие есть узлы AST_
- _Парсинг AST_
- _Итератор_
- _Грамматика JSON_

```scala
val astEt : Either[ParserError,AST] = Parser.parse(
  """{
        a: 1,
        b: {
          c: 2
        }
      }
  """)
```

### Какие есть узлы AST

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


Pretty
-----------------

- _Генерация JSON string_
- _Генерация JSON string pretty_
- _Настройки генерации_

```scala
implicit val fmt = FormattingJson
  .pretty(true)

val json = jsTree.json
println(json)
```

Iterator
-----------------

- _Интератор Token + AST_

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

Query
----------------

- _Запрос к поддереву_

```scala
val astEt = Parser.parse(
  """{
        a: 1,
        b: {
          c: 2
        }
      }
  """)

assert( astEt.query("a").int == Right(1) )
assert( astEt.query("b")("c").int == Right(2) )
```

Derive
-----------

- _FromJson / ToJson_
- _Примеры создания FromJson/ToJson_
- _Пример с простыми полями_
- _Пример с опциональными полями_
- _Пример с значениями по умолчанию_
- _Пример с ADT_
  - _Простой пример ToJson_
  - _Пример ToJson + classTag_
  - _Выбор FromJson на основании query_

```scala
import xyz.cofe.json4s3.derv

assert("123".jsonAs[Int] == Right(123))
assert("1.3".jsonAs[Int] == Right(1))
assert("[1,2]".jsonAs[List[Int]] == Right(List(1,2)))

case class Sample2( a:Option[Int], b:Option[Boolean] )
println( sample2.json )
assert(  sample2b.json.jsonAs[Sample2] == Right(sample2b) )

sealed trait BaseSample
case class ChildOne( a:Int ) extends BaseSample
case class ChildTwo( a:String ) extends BaseSample

import xyz.cofe.json4s3.derv.FromJsonBuilder._
implicit val baseSampleFromJson : FromJson[BaseSample] = 
  FromJson.builder
    .select[ChildOne]( q => q("type").string === "1" )
    .select[ChildTwo]( q => q("type").string === "2" )
    .build

assert( """{ type:"1", a:1 }""".jsonAs[BaseSample] == Right(ChildOne(1)) )
assert( """{ type:"2", a:"xcv" }""".jsonAs[BaseSample] == Right(ChildTwo("xcv")) )
```

Errors
--------------

_Описание иерархии ошибок_