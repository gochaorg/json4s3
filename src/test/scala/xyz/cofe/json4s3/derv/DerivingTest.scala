//package xyz.cofe.json4s3.derv
package xyz.cofe.json4s3
package derv

import xyz.cofe.json4s3.stream.ast.ParserIterator
import xyz.cofe.json4s3.stream.ast.AST
import xyz.cofe.json4s3.stream.ast.AST._

class DerivingTest extends munit.FunSuite:
  test("123 as int") { assert("123".jsonAs[Int] == Right(123)) }
  test("1.3 as int") { assert("1.3".jsonAs[Int] == Right(1)) }
  test("123 as short") { assert("123".jsonAs[Short] == Right(123.toShort)) }
  test("123 as byte") { assert("123".jsonAs[Byte] == Right(123.toByte)) }
  test("1.3 as float") { assert("1.3".jsonAs[Float] == Right(1.3.toFloat)) }
  test("1.3 as double") { assert("1.3".jsonAs[Double] == Right(1.3.toDouble)) }
  test("123 as long") { assert("123".jsonAs[Long] == Right(123.toLong)) }

  test("true as boolean") { assert("true".jsonAs[Boolean] == Right(true)) }
  test("false as boolean") { assert("false".jsonAs[Boolean] == Right(false)) }

  test("[1,2] as List[Int]") {
    println("-------------")
    println("[1,2]".jsonAs[List[Int]])
    assert(
      "[1,2]".jsonAs[List[Int]] == Right(List(1,2))
    )
  }

  test("str to json") { assert( "abc".asJson == Some(JsStr("abc")) ) }
  test("int to json") { assert( 1.asJson == Some(JsInt(1)) ) }
  test("list[1,2] to json") { assert( List(1,2).asJson == Some(JsArray(List(JsInt(1),JsInt(2)))) ) }

  case class Sample1( a:Int, b:String )

  test("case class(a:Int, b:String)") {
    println("="*40)
    println("case class(a:Int, b:String)")

    val sample1 = Sample1(1,"str")
    println(sample1.asJson.map(_.string))

    val sampleJson = "{\"a\":1,\"b\":\"str\"}"
    val sample1et = sampleJson.jsonAs[Sample1]
    assert( sample1et==Right(sample1) )
  }

  case class Sample2( a:Option[Int], b:Option[Boolean] )

  test("case class( a:Option[Int], b:Boolean )") {
    println("="*40)
    println("case class( a:Option[Int], b:Boolean )")

    val sample2 = Sample2(None,Some(true))
    println( sample2.json )
    println( sample2.json.jsonAs[Sample2] )
    assert(  sample2.json.jsonAs[Sample2] == Right(sample2) )

    val sample2b = Sample2(Some(998),Some(false))
    println( sample2b.json )
    println( sample2b.json.jsonAs[Sample2] )
    assert(  sample2b.json.jsonAs[Sample2] == Right(sample2b) )
  }

  sealed trait BaseSample
  case class ChildOne( a:Int ) extends BaseSample
  case class ChildTwo( a:String ) extends BaseSample

  test("builder") {
    import xyz.cofe.json4s3.derv.FromJsonBuilder._
    implicit val baseSampleFromJson : FromJson[BaseSample] = 
      FromJson.builder
        .select[ChildOne]( q => q("type").string === "1" )
        .select[ChildTwo]( q => q("type").string === "2" )
        .build

    //FromJson.builder[BaseSample].map[ChildOne].query("a").string === "2"

    assert( """{ type:"1", a:1 }""".jsonAs[BaseSample] == Right(ChildOne(1)) )
    assert( """{ type:"2", a:"xcv" }""".jsonAs[BaseSample] == Right(ChildTwo("xcv")) )
  }

  case class DefVal( a:String )
  object DefVal:
    given defval:DefaultValue[DefVal] = new DefaultValue[DefVal] {
      override def defaultValue: Option[DefVal] = Some(DefVal("sample"))
    }
  case class ItemWithDef( a:Int, b:DefVal )

  test("default") {
    assert("""{ "a": 1 }""".jsonAs[ItemWithDef] == Right(ItemWithDef(1,DefVal("sample"))))
  }

  test("bignum") {
    assert( 1234567890L.json == "\"1234567890\"" )
  }