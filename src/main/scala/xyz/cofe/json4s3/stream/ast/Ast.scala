package xyz.cofe.json4s3.stream.ast

import xyz.cofe.json4s3.stream.token.Token
import xyz.cofe.json4s3.stream.token.Token

object AST:
  /** Используется при генерации последовательности токенов (из AST дерва в строку) */
  case class GenToken(
    path:List[AST]
  )

  trait JsObjOps:
    def value:List[(String,AST)]
    def get(name:String):Option[AST] = value.find { case(n,ast)=> name==n }.map { case(n,ast)=>ast }

/** Дерево Json */
enum AST:
  case JsStr( value:String )
  case JsFloat( value:Double )
  case JsInt( value:Int )
  case JsBig( value:BigInt )
  case JsNull
  case JsBool( value:Boolean )
  case JsArray( value:Seq[AST] )
  case JsObj( value:List[(String,AST)] ) extends AST with AST.JsObjOps

  /** Возвращает лексемы представляющее данное дерево */
  def tokens(using formatting:FormattingJson):List[Token] = 
    this.tokens0(
      AST.GenToken(
        path = List(this),
      )
    )

  /** Генерирует Json лексемы */
  private def tokens0(state:AST.GenToken)(using fmt:FormattingJson):List[Token] = this match
    case AST.JsStr(value) => List(Token.Str(value))
    case AST.JsFloat(value) => List(Token.FloatNumber(value))
    case AST.JsInt(value) => List(Token.IntNumber(value))
    case AST.JsBig(value) => List(Token.BigNumber(value))
    case AST.JsNull => List(Token.Identifier("null"))
    case AST.JsBool(value) => List(Token.Identifier(
      if(value) "true" else "false"
    ))
    case AST.JsArray(value) => 
      if !fmt.pretty then
        List(Token.OpenSquare) ++ value.zipWithIndex.flatMap { case(a,i) => i match
          case 0 => a.tokens0(state.copy(path = this :: state.path))
          case _ => List(Token.Comma) ++ a.tokens0(state.copy(path = this :: state.path))
        } ++ List(Token.CloseSquare)
      else
        if value.isEmpty then List(Token.OpenSquare, Token.CloseSquare)
        else if value==List(JsArray(List())) then List(Token.OpenSquare, Token.OpenSquare, Token.CloseSquare, Token.CloseSquare)
        else
          val spaceComma = Option.when(fmt.commaSpace.nonEmpty)(List(Token.WhiteSpace(fmt.commaSpace))).toList.flatten

          val items = value.toList.zipWithIndex.flatMap { case (valueAst,idx) => 
            val last = idx==value.size-1
            val prefixTokens = 
              if idx>(-1)
                then List(Token.WhiteSpace(fmt.indent*state.path.length))
                else List()

            val suffixTokens = 
              if last 
                then List(Token.WhiteSpace(fmt.endline))
                else spaceComma ++ List(Token.Comma, Token.WhiteSpace(fmt.endline))

            prefixTokens ++ valueAst.tokens0(state.copy(path = this :: state.path)) ++ suffixTokens
          }

          val prefix = List(Token.OpenSquare, Token.WhiteSpace(fmt.endline))
          val suffix = if state.path.length>1 
            then List(Token.WhiteSpace(fmt.indent*(state.path.length-1)), Token.CloseSquare) 
            else List(Token.CloseSquare)

          prefix ++ items ++ suffix

    case AST.JsObj(value) => 
      if !fmt.pretty then
        List(Token.OpenBrace) ++
        value.zipWithIndex.flatMap { case (((keyStr,valueAst),idx)) => 
          ( if idx>0 then List(Token.Comma) else List() ) ++
          List(Token.Str(keyStr),Token.Colon) ++ valueAst.tokens0(state.copy(path = this::state.path))
        } :+
        Token.CloseBrace
      else
        if value.isEmpty then
          List(Token.OpenBrace, Token.CloseBrace)
        else
          val beforeColon = Option.when(fmt.beforeColon.nonEmpty)(List(Token.WhiteSpace(fmt.beforeColon))).toList.flatten
          val afterColon = Option.when(fmt.afterColon.nonEmpty)(List(Token.WhiteSpace(fmt.afterColon))).toList.flatten
          val spaceComma = Option.when(fmt.commaSpace.nonEmpty)(List(Token.WhiteSpace(fmt.commaSpace))).toList.flatten

          List(Token.OpenBrace) ++ 
          List(Token.WhiteSpace(fmt.endline)) ++
          value.zipWithIndex.flatMap { case( ((keyStr,valueAst),idx) ) => 
            val prefixTokens = List(Token.WhiteSpace(fmt.indent*state.path.length))
            val last = idx==value.size-1
            val suffixTokens = 
              if last then 
                List(Token.WhiteSpace(fmt.endline)) 
              else 
                spaceComma ++ List(Token.Comma,Token.WhiteSpace(fmt.endline))

            prefixTokens ++ 
            List(Token.Str(keyStr)) ++ beforeColon ++ List(Token.Colon) ++ afterColon ++
            valueAst.tokens0(state.copy(path = this :: state.path)) ++
            suffixTokens
          } ++
          ( if state.path.length>1 
              then List(Token.WhiteSpace(fmt.indent*(state.path.length-1))) 
              else List() 
          ) ++
          List(Token.CloseBrace)
  
  /** Возвращает json представление */
  def json(using formatting:FormattingJson):String =
    given fmt : FormattingJson = formatting
    val sb = new StringBuilder
    tokens.foreach { t => sb.append(t.json) }
    sb.toString()

  def string(using formatting:FormattingJson):String = json

  /** Возвращает json представление */
  def toJson(formatting:FormattingJson):String =
    given fmt : FormattingJson = formatting
    json
