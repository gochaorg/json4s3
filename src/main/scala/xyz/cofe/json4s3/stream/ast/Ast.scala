package xyz.cofe.json4s3.stream.ast

import xyz.cofe.json4s3.stream.token.Token
import xyz.cofe.json4s3.stream.token.Token

object AST:
  case class GenToken(
    path:List[AST]
  )

/** Дерево Json */
enum AST:
  case JsStr( value:String )
  case JsFloat( value:Double )
  case JsInt( value:Int )
  case JsBig( value:BigInt )
  case JsNull
  case JsBool( value:Boolean )
  case JsArray( value:Seq[AST] )
  case JsObj( value:Map[String,AST] )

  /** Возвращает лексемы представляющее данное дерево */
  def tokens:List[Token] = 
    this.tokens0(AST.GenToken(path=List(this)))

  private def tokens0(state:AST.GenToken):List[Token] = this match
    case AST.JsStr(value) => List(Token.Str(value))
    case AST.JsFloat(value) => List(Token.FloatNumber(value))
    case AST.JsInt(value) => List(Token.IntNumber(value))
    case AST.JsBig(value) => List(Token.BigNumber(value))
    case AST.JsNull => List(Token.Identifier("null"))
    case AST.JsBool(value) => List(Token.Identifier(
      if(value) "true" else "false"
    ))
    case AST.JsArray(value) => 
      List(Token.OpenSuqare) ++ value.zipWithIndex.flatMap { case(a,i) => i match
        case 0 => a.tokens0(state.copy(path = this :: state.path))
        case _ => List(Token.Comma) ++ a.tokens0(state.copy(path = this :: state.path))
       } ++ List(Token.CloseSuqare)

    case AST.JsObj(value) => 
      if value.isEmpty then
        List(Token.OpenBrace, Token.CloseBrace)
      else
        List(Token.OpenBrace) ++ 
        List(Token.WhiteSpace("\n")) ++
        value.zipWithIndex.flatMap { case( ((keyStr,valueAst),idx) ) => 
          val prefixTokens = List(Token.WhiteSpace("  "*state.path.length))
          val last = idx==value.size-1
          val suffixTokens = 
            if last then 
              List(Token.WhiteSpace("\n")) 
            else 
              List(Token.Comma,Token.WhiteSpace("\n"))

          prefixTokens ++ 
          List(Token.Str(keyStr), Token.Colon) ++ 
          valueAst.tokens0(state.copy(path = this :: state.path)) ++
          suffixTokens
        } ++
        ( if state.path.length>1 
            then List(Token.WhiteSpace("  "*(state.path.length-1))) 
            else List() 
        ) ++
        List(Token.CloseBrace)
  
  /** Возвращает json представление */
  def json:String = 
    val sb = new StringBuilder
    tokens.foreach { t => sb.append(t.json) }
    sb.toString()
  