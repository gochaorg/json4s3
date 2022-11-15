package xyz.cofe.json4s3.stream.ast

import xyz.cofe.json4s3.stream.token.Token
import xyz.cofe.json4s3.stream.token.Token

enum AST:
  case JsStr( value:String )
  case JsFloat( value:Double )
  case JsInt( value:Int )
  case JsBig( value:BigInt )
  case JsNull
  case JsBool( value:Boolean )
  case JsArray( value:Seq[AST] )
  case JsObj( value:Map[String,AST] )

  def tokens:List[Token] = this match
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
        case 0 => a.tokens
        case _ => List(Token.Comma) ++ a.tokens
       } ++ List(Token.CloseSuqare)

    case AST.JsObj(value) => 
      List(Token.OpenBrace) ++ 
      value.zipWithIndex.flatMap { case( ((keyStr,valueAst),idx) ) => idx match
        case 0 => List(Token.Str(keyStr), Token.Colon) ++ valueAst.tokens
        case _ => List(Token.Comma, Token.Str(keyStr), Token.Colon) ++ valueAst.tokens
      } ++
      List(Token.CloseBrace)
  
  def json:String = 
    val sb = new StringBuilder
    tokens.foreach { t => sb.append(t.json) }
    sb.toString()
  