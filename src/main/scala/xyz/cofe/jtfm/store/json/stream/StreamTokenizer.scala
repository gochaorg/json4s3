package xyz.cofe.jtfm.store.json.stream

import scala.reflect.ClassTag

/** 
 * Лекчичисекий анализатор
 * для работы с потоком входящих символов
 */
class StreamTokenizer(using log:StreamTokenizerLogger):
  extension [P <: StreamTokenParserState](state:P)
    def succFinish:Boolean = {
      !state.isError && !state.isAcceptable && state.isReady
    }

  private def stateOf[P <: StreamTokenParser[_]:ClassTag](parser:P, state:parser.STATE):String = {
    var cname = summon[ClassTag[P]].runtimeClass.getName()
    cname = (if cname.contains("stream.string$Pa") then "string"
      else if cname.contains("stream.whitespace$Pa") then "whitespace"
      else if cname.contains("stream.comment$Pa") then "comment"
      else if cname.contains("stream.number$Pa") then "number"
      else if cname.contains("stream.identifier$Pa") then "identifier"
      else if cname.contains("stream.oneCharTokens$Pa") then "oneCharTokens"
      else cname
    ).padTo(20,' ')

    s"$cname state=${state.toString().padTo(50,' ')} "+
    s"${if state.isError then "Error" else "     "} "+
    s"${if state.isAcceptable then "Acceptable" else "          "} "+
    s"${if state.isReady then "Ready" else "     "} "+
    s"${if state.isConsumed then "Consumed" else "        "}"
  }

  private val wsParser = whitespace.Parser()
  private var wsState = wsParser.init

  private val idParser = identifier.Parser()
  private var idState  = idParser.init

  private val strParser = string.Parser()
  private var strState  = strParser.init

  private val oneCharParser = oneCharTokens.Parser()
  private var oneCharState  = oneCharParser.init

  private val numParser = number.Parser()
  private var numState  = numParser.init

  private val cmntParser = comment.Parser()
  private var cmntState  = cmntParser.init

  private var parsers = List(
    parser(wsParser,      wsState,      st =>{wsState=st;st}),
    parser(idParser,      idState,      st =>{idState=st;st}),
    parser(strParser,     strState,     st =>{strState=st;st}),
    parser(oneCharParser, oneCharState, st =>{oneCharState=st;st}),
    parser(numParser,     numState,     st =>{numState=st;st}),
    parser(cmntParser,    cmntState,    st =>{cmntState=st;st}),
  ).toArray

  /** Сброс состояния всех парсеров */
  def resetAll:Unit =
    wsState = wsParser.init
    idState = idParser.init
    strState = strParser.init
    oneCharState = oneCharParser.init
    numState = numParser.init
    cmntState = cmntParser.init

  /** Восстановление состоняния парсеров */
  private def restoreAll:Unit =
    if idState.isError      || idState.succFinish      then idState      = idParser.init
    if wsState.isError      || wsState.succFinish      then wsState      = wsParser.init
    if strState.isError     || strState.succFinish     then strState     = strParser.init
    if oneCharState.isError || oneCharState.succFinish then oneCharState = oneCharParser.init
    if numState.isError     || numState.succFinish     then numState     = numParser.init
    if cmntState.isError    || cmntState.succFinish    then cmntState    = cmntParser.init

  /** Проверка что состояние всех парсеров - fail */
  private def isFailState:Boolean = List(wsState, idState, strState, oneCharState, numState, cmntState).forall { s => s.isError || !s.isAcceptable }

  case class Parsed(oldState:StreamTokenParserState, newState:StreamTokenParserState, tokens:Option[List[Token]])

  private def parser[P <: StreamTokenParser[Char]:ClassTag]( 
    parser:P, readState: =>parser.STATE, writeState:parser.STATE=>parser.STATE 
  )
  :Option[Char]=>Parsed = {
    chrOpt => {
      val oldState = readState
      log(s"char='${chrOpt}' ".padTo(20,' ')+s"${stateOf(parser,oldState)}")
      val newState = chrOpt match
        case None => 
          writeState(parser.end( oldState ))
        case Some(chr) =>
          writeState(parser.accept( oldState, chr ))

      if newState.succFinish then
        log(s"fetched ".padTo(20,' ')+s"${stateOf(parser,readState)}")
        val resultTokens = parser.ready(newState).map(List[Token](_)).getOrElse(List())
        Parsed(oldState,newState,Some(resultTokens))
      else
        log(" ".padTo(20,' ') + s"${stateOf(parser,readState)}")
        Parsed(oldState,newState,None)
    }
  }

  private val failBuffer = new StringBuilder

  /** Принимает очередной символ и генерирует распознаные лексемы (0 или более) */
  def accept(chr:Option[Char]):Either[String,List[Token]] = {
    log("-"*30)
    log( s"accept '$chr'" )

    var stop= false
    var parserIndex = -1
    var toLast = List[Int]()
    var toFirst = List[Int]()
    var results = List[List[Token]]()

    if chr.isEmpty then 
      failBuffer.clear()    
    else
      failBuffer.append(chr.get)

    // парсинг входящего символа
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
              failBuffer.clear()
              resetAll
            else
              if failBuffer.length()>1 then
                failBuffer.delete(0,failBuffer.length()-1)
              restoreAll
          case _ =>
    }

    // перемещение парсеров с error state вниз
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

    if isFailState then
      Left(s"all parsers is in error state, failBuffer \"$failBuffer\"")
    else
      if results.isEmpty then
        Right(List())
      else
        Right(results.flatten)
  }
