json4s3
===========

json4s3 - Это библиотека для парсинга json

Установка
--------------------------

sbt

_пример для sbt_

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

[Основная статья о derive](derive.md)

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

Вычисление типа для json объектов
----------------------------------

Есть простые типы:

- JsNull - соответ литералу null
- JsStr - соответ строковому литералу
- JsFloat
- JsInt
- JsBig
- JsBool

Составные типы
- JsArray - массив
- Prod - "тип-произведение"
  - тип-произведение - это тип к элементу которого можно обратиься по ключу
  - приминительно к JSON это может быть:
    - массив, где ключ - это цело число
    - объект, где ключ - это имя/идентификатор поля объекта
- Sum  - "тип-сумма"
  - тип-сумма - это тип который может быть либо типом А, либо типом Б, либо В ....
    опеределить какого именного типа возможно по функции того или иного языка (например в js это функция typeof() / а для java - оператор instanceOf ) 

Пример:

```scala
val desc =
 // исследуемые json
 List(
   """{ "a": 1, "b":2 }""",
   """{ "a": 123, "c":"abc" }""",
   """{ "a": true, "d":[1,2], "b": { "x": 1 } }""",
 ).map( Parser.parse )
 .map( _.toOption )
 .flatten // Получаем тип List[AST]
 .foldLeft( 
   Prod(Map.empty):JsType // Теперь каждый тип ast будет объеденен с данным
 ){ case (jsType,ast) => jsType.merge(JsonDescribe.describe(ast)) } // объединение типов
```

в результате будет такое

```
{
  a : Sum(
    2 > JsInt
    1 > JsBool 
  )
  b : Sum(
    1 > JsInt
    1 > {      
      x : JsInt      
    } 
  )
  c : JsStr
  d : JsArray
}
```

- `{}` - обозначер Prod тип ("тип-произведение")
- ''имя'' : ''тип'' - описывает тип поля объекта (prod)
- `Sum()` - обозначер Sum тип
- ''число'' > ''тип'' - указывает на элемент типа-суммы и сколько данный тип встретился
    - для типа-произведения число всегда будет 1