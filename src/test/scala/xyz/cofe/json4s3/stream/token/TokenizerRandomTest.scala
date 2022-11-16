package xyz.cofe.json4s3.stream.token

import java.util.concurrent.ThreadLocalRandom

class TokenizerRandomTest extends munit.FunSuite:
  def randIntNum:Token.IntNumber = Token.IntNumber(ThreadLocalRandom.current().nextInt())
  def randBigNum:Token.BigNumber = Token.BigNumber( BigInt(ThreadLocalRandom.current().nextInt()) * BigInt(ThreadLocalRandom.current().nextInt()) )
  def randFloatNum:Token.FloatNumber = Token.FloatNumber( ThreadLocalRandom.current().nextDouble() )
  def randNumber:Token = ThreadLocalRandom.current().nextInt(3) match
    case 0 => randIntNum
    case 1 => randFloatNum
    case _ => randBigNum

  val letters = "qwertyuiopasdfghjklzxcvbnm"

  def irnd(up:Int):Int = ThreadLocalRandom.current().nextInt(up)
  def irnd(min:Int,max:Int):Int = irnd(Math.abs(max-min))+Math.min(min,max)
  def srnd(lenMin:Int, lenMax:Int):String = (0 until (irnd(Math.abs(lenMax - lenMin)) + Math.min(lenMin,lenMax))).map { _ => 
    letters.charAt(irnd(letters.length))
  }.mkString

  def randString:Token.Str =
    Token.Str(srnd(2,10))

  def randComment:Token = irnd(2) match
    case 0 => Token.SLComment(srnd(1,10)+"\n")
    case _ => Token.MLComment(srnd(1,10))

  def randIdent:Token = irnd(3) match
    case 0 => Token.Identifier("true")
    case 1 => Token.Identifier("false")
    case _ => Token.Identifier("null")

  def randWhitespace:Token.WhiteSpace = Token.WhiteSpace(" "*(irnd(10)+1))

  def randOneCharToken:Token = irnd(6) match
    case 0 => Token.CloseBrace
    case 1 => Token.OpenBrace
    case 2 => Token.CloseSuqare
    case 3 => Token.OpenSuqare
    case 4 => Token.Comma
    case _ => Token.Colon

  def randToken:Token = irnd(6) match
    case 0 => randNumber
    case 1 => randString
    case 2 => randString //randComment
    case 3 => randIdent
    case 4 => randWhitespace
    case 5 => randOneCharToken

  def randTokens(minLen:Int, maxLex:Int) =
    (0 until (irnd(minLen,maxLex)))
      .map { _ => randToken }
      .toList
      .foldLeft( List[Token]() ){ case (sum,tok) => 
        sum.lastOption.map {  
          case _:Token.Identifier | _:Token.BigNumber | _:Token.IntNumber | _:Token.FloatNumber => tok match
            case _:Token.Identifier | _:Token.BigNumber | _:Token.IntNumber | _:Token.FloatNumber  =>
              sum :+ Token.WhiteSpace(" ") :+ tok
            case _ =>
              sum :+ tok
          case _:Token.WhiteSpace => tok match
            case _:Token.WhiteSpace =>
              sum :+ Token.IntNumber(irnd(100)) :+ tok
            case _ =>
              sum :+ tok
          case _ =>
            sum :+ tok
        }.orElse( Some(List(tok)) ).get
      }

  test("parse random tokens") {
    val expectTokens = randTokens(50,100)
    val jsonString = expectTokens.map { _.json }.mkString
    val tokenizer = new Tokenizer()

    val actualParsed = tokenizer.parse(jsonString)

    actualParsed match
      case Left(err) => 
        println(s"!!! not parsed $err")

        println(s"json ${jsonString.replace("\r","\\r").replace("\n","\\n")}")

        println("expectTokens")
        expectTokens.map(t => "  "+t.toString().replace("\r","\\r").replace("\n","\\n")).foreach(println)

        //println("actual")
        //actual.map(t => "  "+t.toString().replace("\r","\\r").replace("\n","\\n")).foreach(println)

        fail(err.toString())
      case Right(actualTokens) =>
        println(s"json ${jsonString.replace("\r","\\r").replace("\n","\\n")}")
        
        val allMatched = (actualTokens zip expectTokens).map { case (atoken,etoken) => 
          val matched = atoken == etoken
          println( s"${matched} ${atoken} ${etoken}" )
          matched
        }.forall( t => t )

        if actualTokens.length!=expectTokens.length then
          println(s"actual ${actualTokens.length}")
          actualTokens.map(t => "  "+t).foreach(println)

          println(s"expect ${expectTokens.length}")
          expectTokens.map(t => "  "+t).foreach(println)

          fail("size not matched")

        assert(allMatched,"all matched ?")
  }

  // test("fail 1") {
  //   val expectTokens = List(
  //     //Token.Str("nae"),
  //     // Token.WhiteSpace("        "),
  //     // Token.IntNumber(81),
  //     // Token.WhiteSpace("     "),
  //     // Token.Str("qhrqxfvkd"),
  //     //Token.FloatNumber(0.5501491749404063),
  //     Token.WhiteSpace(" "),
  //     Token.BigNumber(BigInt("-23")),
  //     // Token.WhiteSpace("         "),
  //     // Token.CloseSuqare,
  //     // Token.Str("ppfa"),
  //     // Token.IntNumber(224621680)      ,
  //   )
  //   val jsonString = expectTokens.map { _.json }.mkString
  //   val tokenizer = new Tokenizer()

  //   val actualParsed = tokenizer.parse(jsonString)

  //   actualParsed match
  //     case Left(err) => 
  //       println(s"!!! not parsed $err")

  //       println(s"json ${jsonString.replace("\r","\\r").replace("\n","\\n")}")

  //       println("expectTokens")
  //       expectTokens.map(t => "  "+t.toString().replace("\r","\\r").replace("\n","\\n")).foreach(println)

  //       //println("actual")
  //       //actual.map(t => "  "+t.toString().replace("\r","\\r").replace("\n","\\n")).foreach(println)

  //       fail(err)
  //     case Right(actualTokens) =>
  //       println(s"json ${jsonString.replace("\r","\\r").replace("\n","\\n")}")
        
  //       val allMatched = (actualTokens zip expectTokens).map { case (atoken,etoken) => 
  //         val matched = atoken == etoken
  //         println( s"${matched} ${atoken} ${etoken}" )
  //         matched
  //       }.forall( t => t )

  //       if actualTokens.length!=expectTokens.length then
  //         println(s"actual ${actualTokens.length}")
  //         actualTokens.map(t => "  "+t).foreach(println)
  //         println(s"expect ${expectTokens.length}")
  //         expectTokens.map(t => "  "+t).foreach(println)
  //         fail("size not matched")
  //       assert(allMatched,"all matched ?")
  // }

