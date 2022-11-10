package xyz.cofe.jtfm.store.json.stream

import scala.reflect.ClassTag

object StreamTokenizer:
  extension [P <: StreamTokenParserState](state:P)
    def succFinish:Boolean = {
      !state.isError && !state.isAcceptable && state.isReady
    }

  private def stateOf[P <: StreamTokenParser[_]:ClassTag](parser:P, state:parser.STATE):String = {
    val cname = summon[ClassTag[P]].runtimeClass.getName()
    s"parser $cname state=$state state.isError=${state.isError} state.isAcceptable=${state.isAcceptable} && state.isReady=${state.isReady}"
  }

  private val wsParser = whitespace.Parser()
  private var wsState = wsParser.init

  private val idParser = identifier.Parser()
  private var idState  = idParser.init

  private def printState =
    println( stateOf(wsParser,wsState) )
    println( stateOf(idParser,idState) )

  private def resetAll:Unit =
      idState = idParser.init
      wsState = wsParser.init

  case class Parsed(oldState:StreamTokenParserState, newState:StreamTokenParserState, tokens:Option[List[Token]])

  private def parser[P <: StreamTokenParser[Char]:ClassTag]( 
    parser:P, readState: =>parser.STATE, writeState:parser.STATE=>parser.STATE 
  ):Option[Char]=>Parsed = {
    chrOpt => {
      val oldState = readState
      println(s"parse char='${chrOpt}' ${stateOf(parser,oldState)}")
      val newState = chrOpt match
        case None => 
          writeState(parser.end( oldState ))
        case Some(chr) =>
          writeState(parser.accept( oldState, chr ))

      if newState.succFinish then
        println(s"success ${stateOf(parser,readState)}")
        val resultTokens = parser.ready(newState).map(List[Token](_)).getOrElse(List())
        Parsed(oldState,newState,Some(resultTokens))
      else
        println(s"fail ${stateOf(parser,readState)}")
        Parsed(oldState,newState,None)
    }
  }

  private var parsers = List(
    parser(wsParser, wsState, st =>{wsState=st;st}),
    parser(idParser, idState, st =>{idState=st;st}),
  ).toArray

  def accept(chr:Option[Char]):List[Token] = {
    println("-"*30)
    println( s"accept '$chr'" )

    var stop= false
    var parserIndex = -1
    var toLast = List[Int]()
    var toFirst = List[Int]()
    var results = List[List[Token]]()
    while( !stop ){      
      parserIndex += 1
      if parserIndex>=parsers.length then
        stop = true
      else
        val parsed = parsers(parserIndex)(chr) 
        parsed match
          case Parsed(oldSt,newSt,_) => 
            if !oldSt.isError && newSt.isError then
              toLast = toLast :+ parserIndex
            else if !oldSt.isReady && newSt.isReady then
              toFirst = toFirst :+ parserIndex
        parsed match
          case Parsed(_,_,Some(res)) =>
            results = results :+ res
            if idState.isError || idState.succFinish then idState = idParser.init
            if wsState.isError || wsState.succFinish then wsState = wsParser.init
          case _ =>
    }

    val lastIdx = parsers.length-1
    toLast.foreach { thisIdx => 
      val pThis = parsers(thisIdx)
      val pLast = parsers(lastIdx)
      parsers(lastIdx) = pThis
      parsers(thisIdx) = pLast
    }

    results.headOption.getOrElse(List())
  }
