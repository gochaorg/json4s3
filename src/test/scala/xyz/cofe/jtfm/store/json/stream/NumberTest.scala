package xyz.cofe.jtfm.store.json.stream

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

  test("parse int -12") {
    val parser = number.Parser()
    var state = parser.init

    state = parser.accept(state,'-')
    state = parser.accept(state,'1')
    state = parser.accept(state,'2')
    state = parser.end(state)
    assert( parser.ready(state) == Some(Token.IntNumber(-12)) )
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
