package xyz.cofe.jtfm.store.json.stream

class TokenizerTest extends munit.FunSuite {
  test("parse: ws id") {
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

    val contentMatch = expect.zip(tokens).map { case(exp,act) => 
      val r = exp==act
      println(s"expect $exp actual $act match $r")
      r
    }.forall(x => x)

    assert(sizeMatch)
    assert(contentMatch)
  }

  test("parse: ws id string") {
    println("="*40)
    println("parse: ws id string")

    
    val sample="ab  \"abc\" cde q"
    var tokens = List[Token]()

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
  }
}
