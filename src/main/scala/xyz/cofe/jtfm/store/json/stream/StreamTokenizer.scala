package xyz.cofe.jtfm.store.json.stream

import scala.reflect.ClassTag

object StreamTokenizer:
  extension [P <: StreamTokenParserState](state:P)
    def succFinish:Boolean = {
      !state.isError && !state.isAcceptable && state.isReady
    }

  private def stateOf[P <: StreamTokenParser[_]:ClassTag](parser:P, state:parser.STATE):String = {
    var cname = summon[ClassTag[P]].runtimeClass.getName()
    if cname.contains("stream.string$Pa") then cname = "string"
    else if cname.contains("stream.whitespace$Pa") then cname = "whitespace"
    else if cname.contains("stream.identifier$Pa") then cname = "identifier"
    s"$cname state=$state  Error=${state.isError} Acceptable=${state.isAcceptable} Ready=${state.isReady}"
  }

  private val wsParser = whitespace.Parser()
  private var wsState = wsParser.init

  private val idParser = identifier.Parser()
  private var idState  = idParser.init

  private val strParser = string.Parser()
  private var strState  = strParser.init

  case class Parsed(oldState:StreamTokenParserState, newState:StreamTokenParserState, tokens:Option[List[Token]])

  private def parser[P <: StreamTokenParser[Char]:ClassTag]( 
    parser:P, readState: =>parser.STATE, writeState:parser.STATE=>parser.STATE 
  )(using log:StreamTokenizerLogger)
  :Option[Char]=>Parsed = {
    chrOpt => {
      val oldState = readState
      log(s"char='${chrOpt}' ${stateOf(parser,oldState)}")
      val newState = chrOpt match
        case None => 
          writeState(parser.end( oldState ))
        case Some(chr) =>
          writeState(parser.accept( oldState, chr ))

      if newState.succFinish then
        log(s"fetched ${stateOf(parser,readState)}")
        val resultTokens = parser.ready(newState).map(List[Token](_)).getOrElse(List())
        Parsed(oldState,newState,Some(resultTokens))
      else
        log(s"            ${stateOf(parser,readState)}")
        Parsed(oldState,newState,None)
    }
  }

  private var parsers = List(
    parser(wsParser,  wsState,  st =>{wsState=st;st}),
    parser(idParser,  idState,  st =>{idState=st;st}),
    parser(strParser, strState, st =>{strState=st;st}),
  ).toArray

  def resetAll:Unit =
    wsState = wsParser.init
    idState = idParser.init
    strState = strParser.init

  def accept(chr:Option[Char])(using log:StreamTokenizerLogger):List[Token] = {
    log("-"*30)
    log( s"accept '$chr'" )

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
          case Parsed(_,newSt,Some(res)) =>
            results = results :+ res
            if newSt.isConsumed then
              stop = true
              resetAll
            else
              if idState.isError || idState.succFinish then idState = idParser.init
              if wsState.isError || wsState.succFinish then wsState = wsParser.init
              if strState.isError || strState.succFinish then strState = strParser.init
          case _ =>
    }

    val lastIdx = parsers.length-1
    toLast.sorted.reverse.foreach { thisIdx =>
      val pThis = parsers(thisIdx)
      if thisIdx<(parsers.length-1) then
        var newArr = new Array[Option[Char]=>Parsed](parsers.length)
        (0 until parsers.length-1).foreach { i => 
          if( i>=thisIdx )
            newArr(i) = parsers(i+1) 
          else
            newArr(i) = parsers(i)          
        }
        newArr(newArr.length-1) = pThis
        parsers = newArr
    }

    results.headOption.getOrElse(List())
  }
