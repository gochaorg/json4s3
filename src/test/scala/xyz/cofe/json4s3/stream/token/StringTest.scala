package xyz.cofe.json4s3.stream.token

class StringTest extends munit.FunSuite {
  def check(input:String, expect:String):Boolean = {
    val parser = string.Parser()
    var state = parser.init

    input.foreach { chr => 
      val oldState = state
      state = parser.accept(state,chr) 
      println( s"char='$chr' state $oldState => $state" )
    }

    val oldState = state
    state = parser.end(state)
    println( s"end state $oldState => $state" )

    val res = parser.ready(state)
    val matched = res == Some(Token.Str(expect))
    println(
      (
        if matched then "matched" else "not matched"
      )+
      " input=\""+
      input.replace("\r","\\r").replace("\n","\\n")+
      "\" expect=\""+
      expect.replace("\r","\\r").replace("\n","\\n")+
      "\" res"+(
        if(res.isDefined){
          "\""+res.get.text.replace("\r","\\r").replace("\n","\\n")+"\""
        }else{
          " none"
        }
      )
    )

    matched
  }

  test("parse \"abc\"") {    
    assert( check(
      "\"abc\"", 
      "abc"
    ))
  }

  test("parse \'abc\'") {
    assert( check(
      "\'abc\'", 
      "abc"
    ))
  }

  test("parse \'a\\'bc\'") {
    assert( check(
      "\'a\\'bc\'", 
      "a\'bc"
    ))
  }

  test("parse \'a\\\"bc\'") {
    assert( check(
      "\'a\\\"bc\'", 
      "a\"bc"
    ))
  }

  test("parse \'a\\0\'") {
    assert( check( "\'a\\0\'", "a\u0000" ))
  }

  test("parse \'a\\b\'") {
    assert( check( "\'a\\b\'", "a\u0008" ))
  }

  test("parse \'a\\f\'") {
    assert( check( "\'a\\f\'", "a\u000c" ))
  }

  test("parse \'a\\n\'") {
    assert( check( "\'a\\n\'", "a\u000a" ))
  }

  test("parse \'a\\r\'") {
    assert( check( "\'a\\r\'", "a\u000d" ))
  }

  test("parse \'a\\t\'") {
    assert( check( "\'a\\t\'", "a\u0009" ))
  }

  test("parse \'a\\v\'") {
    assert( check( "\'a\\v\'", "a\u000b" ))
  }

  test("parse oct") {
    assert( check( "\'a\\157\'", "a\u006f" ))
  }

  test("parse hex A1") {
    assert( check( "\'a\\xA1\'", "a\u00a1" ))
  }

  test("parse hex a2") {
    assert( check( "\'a\\xa2\'", "a\u00a2" ))
  }

  test("parse unicode 4") {
    assert( check( "\'a\\u00a2\'", "a\u00a2" ))
  }

  test("parse unicode 5") {
    println("="*40)
    println("parse unicode 5")
    assert( check( "\'a\\u{000a2}\'", "a\u00a2" ))
  }
}

