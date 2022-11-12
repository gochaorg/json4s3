package xyz.cofe.json4s3.stream

class TokenizerTest extends munit.FunSuite {
  test("parse: ws id") {
    println("="*40)
    println("parse: ws id")

    val sample="ab  cde q"
    var tokens = List[Token]()
    val expect = List[Token](
      Token.Identifier("ab"),
      Token.WhiteSpace("  "),
      Token.Identifier("cde"),
      Token.WhiteSpace(" "),
      Token.Identifier("q"),
    )

    val tokenizer = StreamTokenizer()
    sample.foreach { chr =>
      tokenizer.accept(Some(chr)).foreach { _.foreach { tok => 
        println(s"token $tok")
        tokens = tokens :+ tok
      }
    }}
    tokenizer.accept(None).foreach { _.foreach { tok => 
      println(s"token $tok")
      tokens = tokens :+ tok
    }}

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
    val expect = List[Token](
      Token.Identifier("ab"),
      Token.WhiteSpace("  "),
      Token.Str("abc"),
      Token.WhiteSpace(" "),
      Token.Identifier("cde"),
      Token.WhiteSpace(" "),
      Token.Identifier("q"),
    )

    val tokenizer = StreamTokenizer()
    sample.foreach { chr =>
      tokenizer.accept(Some(chr)).foreach { _.foreach { tok => 
        println(s"token $tok")
        tokens = tokens :+ tok
      }
    }}
    tokenizer.accept(None).foreach { _.foreach { tok => 
      println(s"token $tok")
      tokens = tokens :+ tok
    }}

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

  test("parse: ws id string oneCharTokens") {
    println("="*40)
    println("parse: ws id string oneCharTokens")

    val sample="ab ,:[]{} \"abc\" cde q"
    var tokens = List[Token]()
    val expect = List[Token](
      Token.Identifier("ab"),
      Token.WhiteSpace(" "),
      Token.Comma,
      Token.Colon,
      Token.OpenSuqare,
      Token.CloseSuqare,
      Token.OpenBrace,
      Token.CloseBrace,
      Token.WhiteSpace(" "),
      Token.Str("abc"),
      Token.WhiteSpace(" "),
      Token.Identifier("cde"),
      Token.WhiteSpace(" "),
      Token.Identifier("q"),
    )
    
    //given log:StreamTokenizerLogger = StreamTokenizerLogger.stdout

    val tokenizer = StreamTokenizer()
    sample.foreach { chr =>
      tokenizer.accept(Some(chr)).foreach { _.foreach {  tok => 
        println(s"token $tok")
        tokens = tokens :+ tok
      }
    }}
    tokenizer.accept(None).foreach { _.foreach { tok => 
      println(s"token $tok")
      tokens = tokens :+ tok
    }}

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

  test("parse: numbers") {
    println("="*40)
    println("parse: numbers")

    val sample=" 1 2.3 abc -8"
    var tokens = List[Token]()
    val expect = List[Token](
      Token.WhiteSpace(" "),
      Token.IntNumber(1),
      Token.WhiteSpace(" "),
      Token.FloatNumber(2.3),
      Token.WhiteSpace(" "),
      Token.Identifier("abc"),
      Token.WhiteSpace(" "),
      Token.IntNumber(-8),
    )
    
    //given log:StreamTokenizerLogger = StreamTokenizerLogger.stdout

    val tokenizer = StreamTokenizer()
    sample.foreach { chr =>
      tokenizer.accept(Some(chr)).foreach { _.foreach { tok => 
        println(s"token $tok")
        tokens = tokens :+ tok
      }
    }}
    tokenizer.accept(None).foreach { _.foreach { tok => 
      println(s"token $tok")
      tokens = tokens :+ tok
    }}

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

  test("parse: comment") {
    println("="*40)
    println("parse: comment")

    val sample = "  // single line\n" +
                 "// signle two \r\n" +
                 " /* multi line */"
    var tokens = List[Token]()
    val expect = List[Token](
      Token.WhiteSpace("  "),
      Token.SLComment(" single line\n"),
      Token.SLComment(" signle two \r\n"),
      Token.WhiteSpace(" "),
      Token.MLComment(" multi line "),
      // Token.Identifier("abc"),
      // Token.WhiteSpace(" "),
      // Token.IntNumber(-8),
    )
    
    //given log:StreamTokenizerLogger = StreamTokenizerLogger.stdout

    val tokenizer = StreamTokenizer()
    sample.foreach { chr =>
      tokenizer.accept(Some(chr)).foreach { _.foreach { tok => 
        println(s"token ${tok.toString.replace("\r","\\r").replace("\n","\\n")}")
        tokens = tokens :+ tok
      }}
    }
    tokenizer.accept(None).foreach { _.foreach { tok => 
      println(s"token ${tok.toString.replace("\r","\\r").replace("\n","\\n")}")
      tokens = tokens :+ tok
    }}

    println("-"*40)

    val sizeMatch = tokens.length == expect.length
    println(s"size match $sizeMatch")

    val contentMatch = expect.zip(tokens).map { case(exp,act) => 
      val r = exp==act
      println(s"expect ${exp.toString.replace("\r","\\r").replace("\n","\\n")} actual ${act.toString.replace("\r","\\r").replace("\n","\\n")} match $r")
      r
    }.forall(x => x)

    assert(sizeMatch)
    assert(contentMatch)
  }

  test("fail parse") {
    println("="*40)
    println("parse: fail parse")

    val sample = " 1 !!!"

    given log:StreamTokenizerLogger = StreamTokenizerLogger.stdout

    val tokenizer = StreamTokenizer()
    sample.foreach { chr =>
      println(s"char '$chr'")
      tokenizer.accept(Some(chr))
        .left.map( err => println(s"error: $err"))
        .map( _.foreach( tok => {
          println(s"token ${tok.toString.replace("\r","\\r").replace("\n","\\n")}")
        }))
    }
  }
}
