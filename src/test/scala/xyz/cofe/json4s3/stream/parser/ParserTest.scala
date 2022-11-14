package xyz.cofe.json4s3.stream.parser

import xyz.cofe.json4s3.stream.token.Token
import xyz.cofe.json4s3.stream.token.Tokenizer
import xyz.cofe.json4s3.stream.ast._

class ParserTest extends munit.FunSuite:
  test("atomic str") {
    assert( 
      Parser.accept(Parser.State.Init, Token.Str("str123")) == Right(
        Parser.State.Init,
        Some(AST.JsStr("str123"))
      )
    )
  }

  test("atomic int") {
    assert( 
      Parser.accept(Parser.State.Init, Token.IntNumber(1)) == Right(
        Parser.State.Init,
        Some(AST.JsInt(1))
      )
    )
  }  

  test("atomic float") {
    assert( 
      Parser.accept(Parser.State.Init, Token.FloatNumber(2.0)) == Right(
        Parser.State.Init,
        Some(AST.JsFloat(2.0))
      )
    )
  }    

  test("atomic bigint") {
    assert( 
      Parser.accept(Parser.State.Init, Token.BigNumber(3)) == Right(
        Parser.State.Init,
        Some(AST.JsBig(3))
      )
    )
  }

  test("atomic null") {
    assert( 
      Parser.accept(Parser.State.Init, Token.Identifier("null")) == Right(
        Parser.State.Init,
        Some(AST.JsNull)
      )
    )
  }

  test("atomic true") {
    assert( 
      Parser.accept(Parser.State.Init, Token.Identifier("true")) == Right(
        Parser.State.Init,
        Some(AST.JsBool(true))
      )
    )
  }

  test("atomic false") {
    assert( 
      Parser.accept(Parser.State.Init, Token.Identifier("false")) == Right(
        Parser.State.Init,
        Some(AST.JsBool(false))
      )
    )
  }

  test("emptry array") {
    val result = List(
      Token.OpenSuqare,
      Token.CloseSuqare,
    ).foldLeft( Right((Parser.State.Init,None)):Either[String,(Parser.State,Option[AST])] ){ case (sum,tok) => 
      sum.flatMap { case (state, _) => 
        val res = Parser.accept(state,tok)
        println(s"parse $tok => $res")
        res
      }
    }
    
    assert(result.isRight)
    
    val (state,resultJsOpt) = result.getOrElse( (Parser.State.Init, None) )
    assert(resultJsOpt.isDefined)

    assert( resultJsOpt.get == AST.JsArray(List()) )
  }

  test("array[ 1 ]") {
    val result = List(
      Token.OpenSuqare,
      Token.IntNumber(1),
      Token.CloseSuqare,
    ).foldLeft( Right((Parser.State.Init,None)):Either[String,(Parser.State,Option[AST])] ){ case (sum,tok) => 
      sum.flatMap { case (state, _) => 
        val res = Parser.accept(state,tok)
        println(s"parse $tok => $res")
        res
      }
    }
    
    assert(result.isRight)
    
    val (state,resultJsOpt) = result.getOrElse( (Parser.State.Init, None) )
    assert(resultJsOpt.isDefined)

    assert( resultJsOpt.get == AST.JsArray(List( AST.JsInt(1) )) )
  }

  test("array[ 1,2 ]") {
    val result = List(
      Token.OpenSuqare,
      Token.IntNumber(1),
      Token.Comma,
      Token.IntNumber(2),
      Token.CloseSuqare,
    ).foldLeft( Right((Parser.State.Init,None)):Either[String,(Parser.State,Option[AST])] ){ case (sum,tok) => 
      sum.flatMap { case (state, _) => 
        val res = Parser.accept(state,tok)
        println(s"parse $tok => $res")
        res
      }
    }
    
    assert(result.isRight)
    
    val (state,resultJsOpt) = result.getOrElse( (Parser.State.Init, None) )
    assert(resultJsOpt.isDefined)

    assert( resultJsOpt.get == AST.JsArray(List( AST.JsInt(1), AST.JsInt(2) )) )
  }

  test("array[ 1,2, ]") {
    val result = List(
      Token.OpenSuqare,
      Token.IntNumber(1),
      Token.Comma,
      Token.IntNumber(2),
      Token.Comma,
      Token.CloseSuqare,
    ).foldLeft( Right((Parser.State.Init,None)):Either[String,(Parser.State,Option[AST])] ){ case (sum,tok) => 
      sum.flatMap { case (state, _) => 
        val res = Parser.accept(state,tok)
        println(s"parse $tok => $res")
        res
      }
    }
    
    assert(result.isRight)
    
    val (state,resultJsOpt) = result.getOrElse( (Parser.State.Init, None) )
    assert(resultJsOpt.isDefined)

    assert( resultJsOpt.get == AST.JsArray(List( AST.JsInt(1), AST.JsInt(2) )) )
  }

  test("object {}") {
    println("="*40)
    println("object {}")
    
    val result = List(
      Token.OpenBrace,
      Token.CloseBrace,
    ).foldLeft( Right((Parser.State.Init,None)):Either[String,(Parser.State,Option[AST])] ){ case (sum,tok) => 
      sum.flatMap { case (state, _) => 
        val res = Parser.accept(state,tok)
        println(s"parse $tok => $res")
        res
      }
    }
    
    assert(result.isRight)
    
    val (state,resultJsOpt) = result.getOrElse( (Parser.State.Init, None) )
    assert(resultJsOpt.isDefined)

    assert( resultJsOpt.get == AST.JsObj(Map()) )
  }

  test("object {'a':1}") {
    println("="*40)
    println("object {'a':1}")
    
    val result = List(
      Token.OpenBrace,
      Token.Str("a"),
      Token.Colon,
      Token.IntNumber(1),
      Token.CloseBrace,
    ).foldLeft( Right((Parser.State.Init,None)):Either[String,(Parser.State,Option[AST])] ){ case (sum,tok) => 
      sum.flatMap { case (state, _) => 
        val res = Parser.accept(state,tok)
        println(s"parse $tok => $res")
        res
      }
    }
    
    assert(result.isRight)
    
    val (state,resultJsOpt) = result.getOrElse( (Parser.State.Init, None) )
    assert(resultJsOpt.isDefined)

    assert( resultJsOpt.get == AST.JsObj(Map("a"->AST.JsInt(1))) )
  }

  test("object {'a':1,}") {
    println("="*40)
    println("object {'a':1,}")
    
    val result = List(
      Token.OpenBrace,
      Token.Str("a"),
      Token.Colon,
      Token.IntNumber(1),
      Token.Comma,
      Token.CloseBrace,
    ).foldLeft( Right((Parser.State.Init,None)):Either[String,(Parser.State,Option[AST])] ){ case (sum,tok) => 
      sum.flatMap { case (state, _) => 
        val res = Parser.accept(state,tok)
        println(s"parse $tok => $res")
        res
      }
    }
    
    assert(result.isRight)
    
    val (state,resultJsOpt) = result.getOrElse( (Parser.State.Init, None) )
    assert(resultJsOpt.isDefined)

    assert( resultJsOpt.get == AST.JsObj(Map("a"->AST.JsInt(1))) )
  }

  test("object {'a':1,'b':2}") {
    println("="*40)
    println("object {'a':1,'b':2}")
    
    val result = List(
      Token.OpenBrace,
      Token.Str("a"),
      Token.Colon,
      Token.IntNumber(1),
      Token.Comma,
      Token.Str("b"),
      Token.Colon,
      Token.IntNumber(2),
      Token.CloseBrace,
    ).foldLeft( Right((Parser.State.Init,None)):Either[String,(Parser.State,Option[AST])] ){ case (sum,tok) => 
      sum.flatMap { case (state, _) => 
        val res = Parser.accept(state,tok)
        println(s"parse $tok => $res")
        res
      }
    }
    
    assert(result.isRight)
    
    val (state,resultJsOpt) = result.getOrElse( (Parser.State.Init, None) )
    assert(resultJsOpt.isDefined)

    assert( resultJsOpt.get == AST.JsObj(Map("a"->AST.JsInt(1), "b"->AST.JsInt(2))) )
  }
