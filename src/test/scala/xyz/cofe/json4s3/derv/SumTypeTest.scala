package xyz.cofe.json4s3.derv

import xyz.cofe.json4s3.stream.ast.AST
import xyz.cofe.json4s3.stream.ast.AST._
import scala.deriving.*
import scala.compiletime._
import xyz.cofe.json4s3.derv.errors._
import xyz.cofe.json4s3.stream.ast.AST

enum SType:
  case Sym
  case One(a:Int)
  case Two(a:Int,b:String)

class SumTypeTest extends munit.FunSuite:
  test("One(1) to json") {
    println("One(1) to json")
    println("="*60)

    //given der : ToJson[SType] = SumTypeTest.derivedToJson[SType]

    val v : SType = SType.One(1)
    val jsonTreeOpt =  summon[ToJson[SType]].toJson(v)
    println( jsonTreeOpt.map(_.string).getOrElse("?") )

    assert(jsonTreeOpt.isDefined)
    
    val jsonTree = jsonTreeOpt.get
    assert(jsonTree == JsObj(List("One" -> JsObj(List("a" -> JsInt(1))))))
  }

  test("json to One") {
    println("json to One")
    println("="*60)

    val resultEither = """{"One":{"a":1}}""".jsonAs[SType]
    println( resultEither )

    assert( resultEither == Right(SType.One(1)) )
  }

  test("Two(1,2) to json") {
    println("Two(1,2) to json")
    println("="*60)

    val v : SType = SType.Two(1,"abc")
    println( summon[ToJson[SType]].toJson(v).map(_.string).getOrElse("?") )
  }
