package xyz.cofe.json4s3.stream.token

class NumberTest extends munit.FunSuite:
  test("parse int 123") {
    val parser = number.Parser()
    var state = parser.init

    state = parser.accept(state,'1')
    state = parser.accept(state,'2')
    state = parser.accept(state,'3')
    state = parser.end(state)
    assert( parser.ready(state) == Some(Token.IntNumber(123)) )
  }

  test("parse int 0") {
    val parser = number.Parser()
    var state = parser.init

    state = parser.accept(state,'0')
    state = parser.end(state)
    println(parser.ready(state))
    assert( parser.ready(state) == Some(Token.IntNumber(0)) )
  }

  test("parse int -0") {
    val parser = number.Parser()
    var state = parser.init

    state = parser.accept(state,'-')
    state = parser.accept(state,'0')
    state = parser.end(state)
    println(parser.ready(state))
    assert( parser.ready(state) == Some(Token.IntNumber(-0)) )
  }

  test("parse int -12") {
    val parser = number.Parser()
    var state = parser.init

    state = parser.accept(state,'-')
    state = parser.accept(state,'1')
    state = parser.accept(state,'2')
    state = parser.end(state)
    assert( parser.ready(state) == Some(Token.IntNumber(-12)) )
  }

  test("parse float 0.") {
    val parser = number.Parser()
    var state = parser.init

    state = parser.accept(state,'0')
    state = parser.accept(state,'.')
    state = parser.end(state)
    println(parser.ready(state))
    assert( parser.ready(state) == Some(Token.FloatNumber(0.0)) )
  }

  test("parse float 0.0") {
    val parser = number.Parser()
    var state = parser.init

    state = parser.accept(state,'0')
    state = parser.accept(state,'.')
    state = parser.accept(state,'0')
    state = parser.end(state)
    println(parser.ready(state))
    assert( parser.ready(state) == Some(Token.FloatNumber(0.0)) )
  }

  test("parse float 10.5") {
    val parser = number.Parser()
    var state = parser.init

    state = parser.accept(state,'1')
    state = parser.accept(state,'0')
    state = parser.accept(state,'.')
    state = parser.accept(state,'5')
    state = parser.end(state)
    val res = parser.ready(state)
    assert( res == Some(Token.FloatNumber(10.5)) )
  }

  test("parse float -1.5") {
    val parser = number.Parser()
    var state = parser.init

    state = parser.accept(state,'-')
    state = parser.accept(state,'1')
    state = parser.accept(state,'.')
    state = parser.accept(state,'5')
    state = parser.end(state)
    val res = parser.ready(state)
    assert( res == Some(Token.FloatNumber(-1.5)) )
  }

  test("parse float -.5") {
    val parser = number.Parser()
    var state = parser.init

    state = parser.accept(state,'-')
    state = parser.accept(state,'.')
    state = parser.accept(state,'5')
    state = parser.end(state)
    val res = parser.ready(state)
    assert( res == Some(Token.FloatNumber(-.5)) )
  }

  test("parse float 1.2e3") {
    val parser = number.Parser()
    var state = parser.init

    state = parser.accept(state,'1')
    state = parser.accept(state,'.')
    state = parser.accept(state,'2')
    state = parser.accept(state,'e')
    state = parser.accept(state,'3')
    state = parser.end(state)
    val res = parser.ready(state)
    assert( res == Some(Token.FloatNumber(1.2e3)) )
  }

  test("parse float 1.2e+4") {
    val parser = number.Parser()
    var state = parser.init

    state = parser.accept(state,'1')
    state = parser.accept(state,'.')
    state = parser.accept(state,'2')
    state = parser.accept(state,'e')
    state = parser.accept(state,'+')
    state = parser.accept(state,'4')
    state = parser.end(state)
    val res = parser.ready(state)
    assert( res == Some(Token.FloatNumber(1.2e+4)) )
  }

  test("parse float 1.2e-4") {
    val parser = number.Parser()
    var state = parser.init

    state = parser.accept(state,'1')
    state = parser.accept(state,'.')
    state = parser.accept(state,'2')
    state = parser.accept(state,'e')
    state = parser.accept(state,'-')
    state = parser.accept(state,'4')
    state = parser.end(state)
    val res = parser.ready(state)
    val expect = Option(Token.FloatNumber(1.2e-4))
    val matched = res==expect
    //println( s"expect $expect actual $res matched $matched" )
    assert( matched )
  }

  test("parse oct 0153") {
    val parser = number.Parser()
    var state = parser.init

    state = parser.accept(state,'0')
    state = parser.accept(state,'1')
    state = parser.accept(state,'5')
    state = parser.accept(state,'3')
    state = parser.end(state)
    val res = parser.ready(state)
    val expect = Option(Token.IntNumber(1*8*8+5*8+3))
    val matched = res==expect
    assert( matched )
  }

  test("parse oct 0o153") {
    val parser = number.Parser()
    var state = parser.init

    state = parser.accept(state,'0')
    state = parser.accept(state,'o')
    state = parser.accept(state,'1')
    state = parser.accept(state,'5')
    state = parser.accept(state,'3')
    state = parser.end(state)
    val res = parser.ready(state)
    val expect = Option(Token.IntNumber(1*8*8+5*8+3))
    val matched = res==expect
    assert( matched )
  }

  test("parse oct 0O153") {
    val parser = number.Parser()
    var state = parser.init

    state = parser.accept(state,'0')
    state = parser.accept(state,'O')
    state = parser.accept(state,'1')
    state = parser.accept(state,'5')
    state = parser.accept(state,'3')
    state = parser.end(state)
    val res = parser.ready(state)
    val expect = Option(Token.IntNumber(1*8*8+5*8+3))
    val matched = res==expect
    assert( matched )
  }

  test("parse oct 0O153n") {
    val parser = number.Parser()
    var state = parser.init

    state = parser.accept(state,'0')
    state = parser.accept(state,'O')
    state = parser.accept(state,'1')
    state = parser.accept(state,'5')
    state = parser.accept(state,'3')
    state = parser.accept(state,'n')
    state = parser.end(state)
    val res = parser.ready(state)
    val expect = Option(Token.BigNumber(1*8*8+5*8+3))
    val matched = res==expect
    assert( matched )
  }

  test("parse oct 0153n") {
    val parser = number.Parser()
    var state = parser.init

    state = parser.accept(state,'0')
    state = parser.accept(state,'1')
    state = parser.accept(state,'5')
    state = parser.accept(state,'3')
    state = parser.accept(state,'n')
    state = parser.end(state)
    val res = parser.ready(state)
    val expect = Option(Token.BigNumber(1*8*8+5*8+3))
    val matched = res==expect
    assert( matched )
  }

  test("parse hex 0xfa2") {
    val parser = number.Parser()
    var state = parser.init

    state = parser.accept(state,'0')
    state = parser.accept(state,'x')
    state = parser.accept(state,'f')
    state = parser.accept(state,'a')
    state = parser.accept(state,'2')
    state = parser.end(state)
    val res = parser.ready(state)
    val expect = Option(Token.IntNumber(15*16*16+10*16+2))
    val matched = res==expect
    assert( matched )
  }

  test("parse hex 0xfa2n") {
    val parser = number.Parser()
    var state = parser.init

    state = parser.accept(state,'0')
    state = parser.accept(state,'x')
    state = parser.accept(state,'f')
    state = parser.accept(state,'a')
    state = parser.accept(state,'2')
    state = parser.accept(state,'n')
    state = parser.end(state)
    val res = parser.ready(state)
    val expect = Option(Token.BigNumber(15*16*16+10*16+2))
    val matched = res==expect
    assert( matched )
  }

  test("parse hex 0Xfa2n") {
    val parser = number.Parser()
    var state = parser.init

    state = parser.accept(state,'0')
    state = parser.accept(state,'X')
    state = parser.accept(state,'f')
    state = parser.accept(state,'a')
    state = parser.accept(state,'2')
    state = parser.accept(state,'n')
    state = parser.end(state)
    val res = parser.ready(state)
    val expect = Option(Token.BigNumber(15*16*16+10*16+2))
    val matched = res==expect
    assert( matched )
  }

  test("parse hex 0Xfa") {
    val parser = number.Parser()
    var state = parser.init

    state = parser.accept(state,'0')
    state = parser.accept(state,'X')
    state = parser.accept(state,'f')
    state = parser.accept(state,'a')
    state = parser.end(state)
    val res = parser.ready(state)
    val expect = Option(Token.IntNumber(15*16+10))
    val matched = res==expect
    assert( matched )
  }

  test("parse hex 0XFA") {
    val parser = number.Parser()
    var state = parser.init

    state = parser.accept(state,'0')
    state = parser.accept(state,'X')
    state = parser.accept(state,'F')
    state = parser.accept(state,'A')
    state = parser.end(state)
    val res = parser.ready(state)
    val expect = Option(Token.IntNumber(15*16+10))
    val matched = res==expect
    assert( matched )
  }

  test("parse hex 0b110") {
    val parser = number.Parser()
    var state = parser.init

    state = parser.accept(state,'0')
    state = parser.accept(state,'b')
    state = parser.accept(state,'1')
    state = parser.accept(state,'1')
    state = parser.accept(state,'0')
    state = parser.end(state)
    val res = parser.ready(state)
    val expect = Option(Token.IntNumber(1*4+1*2+0))
    val matched = res==expect
    assert( matched )
  }

  test("parse hex 0B110") {
    val parser = number.Parser()
    var state = parser.init

    state = parser.accept(state,'0')
    state = parser.accept(state,'B')
    state = parser.accept(state,'1')
    state = parser.accept(state,'1')
    state = parser.accept(state,'0')
    state = parser.end(state)
    val res = parser.ready(state)
    val expect = Option(Token.IntNumber(1*4+1*2+0))
    val matched = res==expect
    assert( matched )
  }

  test("parse hex 0b110n") {
    val parser = number.Parser()
    var state = parser.init

    state = parser.accept(state,'0')
    state = parser.accept(state,'b')
    state = parser.accept(state,'1')
    state = parser.accept(state,'1')
    state = parser.accept(state,'0')
    state = parser.accept(state,'n')
    state = parser.end(state)
    val res = parser.ready(state)
    val expect = Option(Token.BigNumber(1*4+1*2+0))
    val matched = res==expect
    assert( matched )
  }
