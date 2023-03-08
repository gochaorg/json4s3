Derive
=================

Механизм для автомитической генерации кода, 
в данном случае для поддержки сериализации и де-сериализации Json

- _FromJson / ToJson_
- _Примеры создания FromJson/ToJson_
- _Пример с простыми полями_
- _Пример с опциональными полями_
- _Пример с значениями по умолчанию_
- _Пример с ADT_
  - _Простой пример ToJson_
  - _Пример ToJson + classTag_
  - _Выбор F- _FromJson / ToJson_
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

Поддержка "суммы-типов"

```scala
enum SType:
  case Sym
  case One(a:Int)
  case Two(a:Int,b:String)

val resultEither = """{"One":{"a":1}}""".jsonAs[SType]
assert( resultEither == Right(SType.One(1)) )
```
