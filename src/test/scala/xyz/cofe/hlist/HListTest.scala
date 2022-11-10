package xyz.cofe.hlist

class HListTest extends munit.FunSuite {
  test("hlist") {
    import HList._

    //val third: Head[String, Head[Boolean, Head[Int, Nil]]] = "third" :: true :: 10 :: nil
    val third = "third" :: true :: 10 :: nil

    println("start")
    println(third.find[String])
    println(third.find[Boolean])
    println(third.find[Int])
    println("finish")
  }
}
