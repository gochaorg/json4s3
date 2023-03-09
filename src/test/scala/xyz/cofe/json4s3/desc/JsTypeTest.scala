package xyz.cofe.json4s3.desc

import JsType._

class JsTypeTest extends munit.FunSuite:
  test("merge 1") {
    val t1 = JsType.JsNull.merge(JsType.JsNull)
    println(t1)

    val t2 = JsType.JsNull.merge(JsType.JsStr)
    println(t2)

    val t3 = t1.merge(t2)
    println(t3)
  }

  test("prod merge") {
    println("prod merge")
    println("="*60)
    val p1 = Prod(Map("a"->JsNull, "b"->JsInt))
    val p2 = Prod(Map("a"->JsBool, "c"->JsStr))
    val p3 = p1.merge(p2)
    println(p3)
  }

  test("prod merge atom") {
    println("prod merge atom")
    println("="*60)
    val p1 = Prod(Map("a"->JsNull, "b"->JsInt))
    val p2 = JsStr
    val p3 = p1.merge(p2)
    println(p3)
  }  

  test("prod merge sum") {
    println("prod merge sum")
    println("="*60)
    val p1 = Prod(Map("a"->JsNull, "b"->JsInt))
    val p2 = JsStr.merge(JsBool)
    val p3 = p1.merge(p2)
    println(p3)
  }    

  test("nested prod merge") {
    println("nested prod merge")
    println("="*60)
    val p1 = Prod(Map("a"->JsNull, "b"->JsStr))
    val p2 = Prod(Map("c"->p1))

    val p3 = Prod(Map("a"->JsNull, "c"->JsStr))
    val p4 = Prod(Map("c"->p3, "d"->JsInt))

    val p5 = p4.merge(p2)
    println(p5)
  }