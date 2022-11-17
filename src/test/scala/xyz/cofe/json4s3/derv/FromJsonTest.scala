//package xyz.cofe.json4s3.derv
package xyz.cofe.json4s3
package derv

import xyz.cofe.json4s3.stream.ast.ParserIterator

class FromJsonTest extends munit.FunSuite:
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
    assert(
      "[1,2]".jsonAs[List[Int]] == Right(List(1,2))
    )
  }
