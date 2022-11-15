package xyz.cofe.json4s3.stream.parser

import java.util.concurrent.ThreadLocalRandom
import xyz.cofe.json4s3.stream.ast.AST._
import xyz.cofe.json4s3.stream.ast.AST
import xyz.cofe.json4s3.stream.ast.Parser

class ParserRandomTest extends munit.FunSuite:
  object rnd {
    private val r = ThreadLocalRandom.current()

    def int:Int = r.nextInt()
    def double:Double = r.nextDouble()
    def int(limit:Int) = r.nextInt(limit)
    def int(min:Int,max:Int) = r.nextInt( Math.abs(max-min) ) + Math.min(max,min)
    def bigInt:BigInt = BigInt( int ) * BigInt( int )

    val lowLetters = "qwertyuiopasdfghjklzxcvbnm"
    lazy val hiLetters = lowLetters.toUpperCase()
    lazy val letters = hiLetters ++ lowLetters

    def string(minLen:Int,maxLen:Int) = (0 until int(minLen,maxLen)).map { _ =>
      letters.charAt(int(letters.length))
    }.mkString
  }

  object ast {
    case class Opt(
      deep:Int=3, 
      width:Int=3, 
      level:Int=0,
      rootStructOnly:Boolean=true
    )

    def str = JsStr(rnd.string(0,10))
    def int = JsInt(rnd.int)
    def float = JsFloat(rnd.double)
    def bigint = JsBig(rnd.bigInt)
    def bool = rnd.int match
      case 0 => JsBool(false)
      case _ => JsBool(true)      

    def js(opt:Opt=Opt()):AST = 
      rnd.int( if opt.rootStructOnly && opt.level==0 then 2 else 8 ) match
        case 0 => opt.deep match
          case _ if opt.deep<=0 => AST.JsArray(List())
          case _ =>
            AST.JsArray( (0 until opt.width).map { _ => js(opt.copy(deep=opt.deep-1, level=opt.level+1)) }.toList )
        case 1 => opt.deep match
          case _ if opt.deep<=0 => AST.JsObj(Map())
          case _ =>
            AST.JsObj( (0 until opt.width).map { _ => rnd.string(1,5) -> js(opt.copy(deep=opt.deep-1, level=opt.level+1)) }.toMap )
        case 2 => str
        case 3 => int
        case 4 => float
        case 5 => bigint
        case 6 => bool
        case 7 => AST.JsNull
  }

  test("try parse") {
    val astJs = ast.js()
    (0 until 50).foreach { _=> 
      Parser.parse( astJs.tokens ) match
        case Left(err) => fail(err)
        case Right(value) => ()
    }
  }