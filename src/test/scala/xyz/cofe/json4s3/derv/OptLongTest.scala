package xyz.cofe.json4s3.derv

class OptLongTest extends munit.FunSuite:
  case class Sample(value:Option[Long])

  test("store opt long") {
    List( 100L, 1000L, 10000L, 40000L ).map( n =>
      val jsn = Sample(Some(n)).json
      val v = jsn.jsonAs[Sample]
      println( s"$n $jsn $v")
    )
  }
