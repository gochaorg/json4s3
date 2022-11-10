package xyz.cofe.jtfm.store.json.stream

class TokenizerTest extends munit.FunSuite {
  test("parse") {
    val sample="ab  cde q"
    var tokens = List[Token]()
    val expect = List[Token](
      Token.Identifier("ab"),
      Token.WhiteSpace("  "),
      Token.Identifier("cde"),
      Token.WhiteSpace(" "),
      Token.Identifier("q"),
    )
    sample.foreach { chr =>
      StreamTokenizer.accept(Some(chr)).foreach { tok => 
        println(s"token $tok")
        tokens = tokens :+ tok
      }
    }
    StreamTokenizer.accept(None).foreach { tok => 
      println(s"token $tok")
      tokens = tokens :+ tok
    }

    println("-"*40)
    val sizeMatch = tokens.length == expect.length
    println(s"size match $sizeMatch")
    expect.zip(tokens).map { case(exp,act) => 
      val r = exp==act
      println(s"expect $exp actual $act match $r")
      r
    }
  }

  trait Show[T]:
    def show(t:T):String
  
  object Show:
    given Show[Int] with
      def show(i:Int) = s"$i"

    given Show[EmptyTuple] with
      def show(e:EmptyTuple)=""

    given [H:Show,T<:Tuple:Show]:Show[H *: T] with
      def show(t: H *: T):String = 
        summon[Show[H]].show(t.head) + ", " + summon[Show[T]].show(t.tail)

  extension [T:Show](t:T)
    def show:String = summon[Show[T]].show(t)

  test("show test") {
    println("===================")
    println("=== show test  ====")
    println( (5, 6, 1, 2, 3).show )
  }
}
