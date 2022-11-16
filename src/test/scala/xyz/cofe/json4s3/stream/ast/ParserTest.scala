package xyz.cofe.json4s3.stream.ast

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
        res.left.map(_.toString())
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
        res.left.map(_.toString())
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
        res.left.map(_.toString())
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
        res.left.map(_.toString())
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
        res.left.map(_.toString())
      }
    }
    
    assert(result.isRight)
    
    val (state,resultJsOpt) = result.getOrElse( (Parser.State.Init, None) )
    assert(resultJsOpt.isDefined)

    assert( resultJsOpt.get == AST.JsObj(List()) )
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
        res.left.map(_.toString())
      }
    }
    
    assert(result.isRight)
    
    val (state,resultJsOpt) = result.getOrElse( (Parser.State.Init, None) )
    assert(resultJsOpt.isDefined)

    assert( resultJsOpt.get == AST.JsObj(List("a"->AST.JsInt(1))) )
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
        res.left.map(_.toString())
      }
    }
    
    assert(result.isRight)
    
    val (state,resultJsOpt) = result.getOrElse( (Parser.State.Init, None) )
    assert(resultJsOpt.isDefined)

    assert( resultJsOpt.get == AST.JsObj(List("a"->AST.JsInt(1))) )
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
        res.left.map(_.toString())
      }
    }
    
    assert(result.isRight)
    
    val (state,resultJsOpt) = result.getOrElse( (Parser.State.Init, None) )
    assert(resultJsOpt.isDefined)

    assert( resultJsOpt.get == AST.JsObj(List("a"->AST.JsInt(1), "b"->AST.JsInt(2))) )
  }

  test("json {'a':1,'b':2}") {
    println("="*40)
    println("json {'a':1,'b':2}")

    Tokenizer.parse("{'a':1,'b':2}").getOrElse(List())
    
    val result = Tokenizer
    .parse("{'a':1,'b':2}")
    .getOrElse(List())
    .foldLeft( Right((Parser.State.Init,None)):Either[String,(Parser.State,Option[AST])] ){ case (sum,tok) => 
      sum.flatMap { case (state, _) => 
        val res = Parser.accept(state,tok)
        println(s"parse $tok => $res")
        res.left.map(_.toString())
      }
    }
    
    assert(result.isRight)
    
    val (state,resultJsOpt) = result.getOrElse( (Parser.State.Init, None) )
    assert(resultJsOpt.isDefined)

    assert( resultJsOpt.get == AST.JsObj(List("a"->AST.JsInt(1), "b"->AST.JsInt(2))) )
  }

  test("json {'a':1,'b':[]}") {
    println("="*40)
    println("object {'a':1,'b':[]}")

    val result = Tokenizer
    .parse("{'a':1,'b':[]}")
    .getOrElse(List())
    .foldLeft( Right((Parser.State.Init,None)):Either[String,(Parser.State,Option[AST])] ){ case (sum,tok) => 
      sum.flatMap { case (state, _) => 
        val res = Parser.accept(state,tok)
        println(s"parse $tok => $res")
        res.left.map(_.toString())
      }
    }
    
    assert(result.isRight)
    
    val (state,resultJsOpt) = result.getOrElse( (Parser.State.Init, None) )
    assert(resultJsOpt.isDefined)

    assert( resultJsOpt.get == AST.JsObj(List("a"->AST.JsInt(1), "b"->AST.JsArray(List()))) )
  }

  test("json {'a':1,'b':[1]}") {
    println("="*40)
    println("object {'a':1,'b':[1]}")

    val result = Tokenizer
    .parse("{'a':1,'b':[1]}")
    .getOrElse(List())
    .foldLeft( Right((Parser.State.Init,None)):Either[String,(Parser.State,Option[AST])] ){ case (sum,tok) => 
      sum.flatMap { case (state, _) => 
        val res = Parser.accept(state,tok)
        println(s"parse $tok => $res")
        res.left.map(_.toString())
      }
    }
    
    assert(result.isRight)
    
    val (state,resultJsOpt) = result.getOrElse( (Parser.State.Init, None) )
    assert(resultJsOpt.isDefined)

    assert( resultJsOpt.get == AST.JsObj(
      List(
        "a"->AST.JsInt(1), 
        "b"->AST.JsArray(List(AST.JsInt(1)))
      )
    ))
  }

  val json_obj1 = "{'a':1,'b':[1,{}]}"
  test(s"json {'a':1,'b':[1,{}]}") {
    println("="*40)
    println(s"json $json_obj1")

    val result = Tokenizer
    .parse(json_obj1)
    .getOrElse(List())
    .foldLeft( Right((Parser.State.Init,None)):Either[String,(Parser.State,Option[AST])] ){ case (sum,tok) => 
      sum.flatMap { case (state, _) => 
        val res = Parser.accept(state,tok)
        println(s"parse $tok => $res")
        res.left.map(_.toString())
      }
    }
    
    assert(result.isRight)
    
    val (state,resultJsOpt) = result.getOrElse( (Parser.State.Init, None) )
    assert(resultJsOpt.isDefined)

    assert( resultJsOpt.get == AST.JsObj(
      List(
        "a"->AST.JsInt(1), 
        "b"->AST.JsArray(List(
          AST.JsInt(1),
          AST.JsObj(List())
        ))
      )
    ))
  }

  val json_obj2 = "{'a':1,'b':[2,{'c':3}]}"
  test(s"json {'a':1,'b':[2,{'c':3}]}") {
    println("="*40)
    println(s"json $json_obj2")

    val tokens = Tokenizer.parse(json_obj2).getOrElse(List())
    println("tokens:")
    tokens.map { t => "  "+t }.foreach(println)

    val result = 
      tokens
      .foldLeft( Right((Parser.State.Init,None)):Either[String,(Parser.State,Option[AST])] ){ case (sum,tok) => 
        sum.flatMap { case (state, _) => 
          val res = Parser.accept(state,tok)
          println(s"parse $tok => $res")
          res.left.map(_.toString())
        }
      }
    
    assert(result.isRight)
    
    val (state,resultJsOpt) = result.getOrElse( (Parser.State.Init, None) )
    assert(resultJsOpt.isDefined)

    assert( resultJsOpt.get == AST.JsObj(
      List(
        "a"->AST.JsInt(1), 
        "b"->AST.JsArray(List(
          AST.JsInt(2),
          AST.JsObj(List(
            "c"->AST.JsInt(3)
          ))
        ))
      )
    ))
  }

  val json_obj3 = "{a:[[]]}"
  test(s"json {a:[[]]}") {
    println("="*40)
    println(s"json $json_obj3")

    val tokens = Tokenizer.parse(json_obj3).getOrElse(List())
    println("tokens:")
    tokens.map { t => "  "+t }.foreach(println)

    val result = 
      tokens
      .foldLeft( Right((Parser.State.Init,None)):Either[String,(Parser.State,Option[AST])] ){ case (sum,tok) => 
        sum.flatMap { case (state, _) => 
          val res = Parser.accept(state,tok)
          println(s"parse $tok => $res")
          res.left.map(_.toString())
        }
      }
    
    assert(result.isRight)
    
    val (state,resultJsOpt) = result.getOrElse( (Parser.State.Init, None) )
    assert(resultJsOpt.isDefined)

    assert( resultJsOpt.get == AST.JsObj(
      List(
        "a"->AST.JsArray(
          List(AST.JsArray(List()))
        ), 
      )
    ))
  }
