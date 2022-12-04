json4s3
===========

json4s3 - Это библиотека для парсинга json

lexem parse (tokenizer)
--------------------------

- _Какие есть лексемы_
- _Парсер лексем_
- _Итератор по лексемам_
- _Вложенный итератор по лексемам_

```scala
assert(TokenIterator("123 true false").toList == List(
  Token.IntNumber(123),
  Token.WhiteSpace(" "),
  Token.Identifier("true"),
  Token.WhiteSpace(" "),
  Token.Identifier("false"),
))
```

ast parse
-------------------

- _Какие есть узлы AST_
- _Парсинг AST_
- _Итератор_
- _Грамматика JSON_

```scala
val astEt = Parser.parse(
  """{
        a: 1,
        b: {
          c: 2
        }
      }
  """)
```

pretty
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

iterator
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

query
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

derive
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

errors
--------------

_Описание иерархии ошибок_