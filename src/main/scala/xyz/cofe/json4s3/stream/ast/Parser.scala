package xyz.cofe.json4s3.stream.ast

import xyz.cofe.json4s3.stream.token.Token
import xyz.cofe.json4s3.stream.token.Token
import xyz.cofe.json4s3.stream.token.Token
import xyz.cofe.json4s3.stream.token.Token
import xyz.cofe.json4s3.stream.token.Token
import xyz.cofe.json4s3.stream.token.Token

/**
 * = grammar =
 * 
 *     grammar ::= expression { expression }
 * 
 *     expression ::= { skipToken } ( string | number | null | bool | array | object )
 * 
 *     skipToken ::= Token.SLComment | Token.MLComment | Token.WhiteSpace
 * 
 *     string ::= AST.JsStr
 *     number ::= AST.JsFloat | AST.JsInt | AST.JsBig
 * 
 *     AST.JsStr ::= Token.JsStr
 * 
 *     AST.JsFloat ::= Token.FloatNumber
 *     AST.JsInt   ::= Token.IntNumber
 *     AST.JsBig   ::= Token.BigNumber
 * 
 *     null ::= Token.Identifier(null)
 *     bool ::= Token.Identifier(true) |  Token.Identifier(false)
 * 
 *     array ::= Token.OpenSuqare expression { Token.Comma expression } [ Token.Comma ] Token.CloseSuqare
 *             | Token.OpenSuqare [ Token.Comma ] Token.CloseSuqare
 * 
 *     object ::= Token.OpenBrace fieldKeyValue { Token.Comma fieldKeyValue } [ Token.Comma ] Token.CloseSuqare
 *              | Token.OpenBrace [ Token.Comma ] Token.CloseSuqare
 * 
 *     fieldKeyValue ::= ( fieldName {skipToken} Token.Colon expression )
 */
object Parser:
  enum State:
    // Str         -> Init out:JsStr
    // IntNumber   -> Init out:JsInt
    // BigNumber   -> Init out:JsBig
    // FloatNumber -> Init out:JsFloat
    // SLComment   -> Init skip
    // MLComment   -> Init skip
    // WhiteSpace  -> Init skip
    // OpenSuqare  -> ArrExpectValue
    // CloseSuqare -> Err
    // OpenBrace   -> ObjExpFieldName
    // CloseBrace  -> Err
    // Comma       -> Err
    // Colon       -> Err
    // Identifier  -> ? true | false -> JsBool
    //             -> ? null         -> JsNull
    //             -> Err
    case Init extends State

    // SLComment | MLComment | WhiteSpace  -> ArrExpectValue skip
    //                                     -> Init( current )     -> ArrExpectComma
    case ArrExpectValue( value:List[AST], parent:Option[State]=None ) extends State with ArrayOps

    // SLComment | MLComment | WhiteSpace  -> ArrExpectComma
    // Comma       -> ArrAfterComma
    // CloseSquare -> pop
    //             -> Err
    case ArrExpectComma( value:List[AST], parent:Option[State]=None ) extends State with ArrayOps

    // SLComment | MLComment | WhiteSpace  -> ArrAfterComma
    // CloseSquare -> pop
    //             -> ArrExpectValue( current ) -> ArrExpectComma
    //             -> Err
    case ArrAfterComma( value:List[AST], parent:Option[State]=None ) extends State with ArrayOps

    // SLComment | MLComment | WhiteSpace  -> ArrAfterComma
    // Identifier | Str -> ObjAfterFieldName
    //                  -> Err
    case ObjExpFieldName( value:Map[String,AST], parent:Option[State]=None ) extends State with ObjOps

    // SLComment | MLComment | WhiteSpace  -> ObjExpFieldValue
    // Colom -> ObjExpFieldValue
    //       -> Err
    case ObjAfterFieldName( fieldName:String, value:Map[String,AST], parent:Option[State]=None ) extends State with ObjOps

    // SLComment | MLComment | WhiteSpace -> ObjExpFieldValue skip
    //                                    -> Init( current )       -> ObpExpComma
    case ObjExpFieldValue( fieldName:String, value:Map[String,AST], parent:Option[State]=None ) extends State with ObjOps

    // SLComment | MLComment | WhiteSpace -> ObpExpComma skip
    // Comma                              -> ObjAfterComma
    // CloseBrace                         -> pop
    //                                    -> Err
    case ObjExpectComma( value:Map[String,AST], parent:Option[State]=None ) extends State with ObjOps

    // SLComment | MLComment | WhiteSpace -> ObjAfterComma skip
    // CloseBrace                         -> pop
    //                                    -> ObjExpFieldName( current )
    case ObjAfterComma( value:Map[String,AST], parent:Option[State]=None ) extends State with ObjOps

  trait ArrayOps:
    def value:List[AST]
    def toJsArray:AST.JsArray = AST.JsArray(value)

  trait ObjOps:
    def value:Map[String,AST]
    def toJsObj:AST.JsObj = AST.JsObj(value)

  extension (parentOpt:Option[State])
    def acceptArray(s:State, token:Token, value:List[AST]):Either[String,(State,Option[AST])] =
      parentOpt.getOrElse(State.Init) match
        case State.Init => 
          Right((State.Init, Some(AST.JsArray(value))))
        case State.ArrExpectValue(arr, parent) =>
          // добавление элемента в родительский массив и переход к запятой
          Right((State.ArrExpectComma(arr ++ value,parent), None))
        case State.ObjExpFieldValue(fieldName, fields, parent) =>
          // добавление элемента в объект и переход к запятой
          Right(( State.ObjExpectComma( fields + (fieldName -> (AST.JsArray(value))), parent), None ))
        case _ =>
          Left(s"fail state=$s accept $token")
    def acceptObject(state:State, token:Token, value:Map[String,AST]):Either[String,(State,Option[AST])] =
      parentOpt.getOrElse(State.Init) match
        case State.Init => 
          Right((State.Init, Some(AST.JsObj(value))))
        case State.ArrExpectValue(arr, parent) =>
          Right((
            State.ArrExpectComma(arr :+ AST.JsObj(value), parent),
            None
          ))
        case State.ObjExpFieldValue(fieldName, fields, parent) =>
          Right((
            State.ObjExpectComma(
              fields + (fieldName -> AST.JsObj(value))
              ,parent
            ),
            None
          ))
        case _ =>
          Left(s"fail state=$state accept $token")


  
  def accept(state:State, token:Token):Either[String,(State,Option[AST])] = state match
    case State.Init => token match
      case Token.Str(text) => Right((State.Init,Some(AST.JsStr(text))))
      case Token.IntNumber(num) => Right((State.Init,Some(AST.JsInt(num))))
      case Token.BigNumber(num) => Right((State.Init,Some(AST.JsBig(num))))
      case Token.FloatNumber(num) => Right((State.Init,Some(AST.JsFloat(num))))
      case Token.Identifier(text) => text match
        case "true" =>  Right((State.Init,Some(AST.JsBool(true))))
        case "false" =>  Right((State.Init,Some(AST.JsBool(false))))
        case "null" =>  Right((State.Init,Some(AST.JsNull)))
        case _ => Left(s"undefined identifier $text")
      case Token.OpenSuqare => Right((State.ArrExpectValue(List(),Some(state))), None)
      case Token.OpenBrace => Right((State.ObjExpFieldName(Map(),Some(state)), None))
      case Token.WhiteSpace(_) => Right((State.Init,None))
      case Token.SLComment(_) => Right((State.Init,None))
      case Token.MLComment(_) => Right((State.Init,None))
      case _ => Left(s"not allowed token $token")
    
    // Ожидание эелемента массива
    case s@State.ArrExpectValue(value,parentOpt) => token match
      case Token.Str(text) => 
        Right(( State.ArrExpectComma(value :+ AST.JsStr(text)),None ))
      case Token.IntNumber(num) =>
        Right(( State.ArrExpectComma(value :+ AST.JsInt(num)),None ))
      case Token.BigNumber(num) =>
        Right(( State.ArrExpectComma(value :+ AST.JsBig(num)),None ))
      case Token.FloatNumber(num) =>
        Right(( State.ArrExpectComma(value :+ AST.JsFloat(num)),None ))
      case Token.Identifier(text) => text match
        case "true" =>
          Right(( State.ArrExpectComma( value :+ AST.JsBool(true) ),None ))
        case "false" =>
          Right(( State.ArrExpectComma( value :+ AST.JsBool(false)),None ))
        case "null" =>
          Right(( State.ArrExpectComma(value :+ AST.JsNull),None ))
      case Token.OpenSuqare =>
        Right(( State.ArrExpectValue(List(),Some(state)),None ))
      case Token.OpenBrace =>
        Right((
          State.ObjExpFieldName(Map(),Some(state)),
          None
        ))

      // Завершение массива
      case Token.CloseSuqare =>
        parentOpt.acceptArray(state, token, value)
      case Token.CloseBrace | Token.Comma | Token.Colon =>
        Left(s"expect value, but accept $token")
      case Token.WhiteSpace(_) | Token.SLComment(_) | Token.MLComment(_) =>
        Right((state,None))
    
    // Ожидание запятой разделяющей элементы массива или закрытия скобки - конец массива
    case s@State.ArrExpectComma(value,parentOpt) => token match
      case Token.WhiteSpace(_) | Token.SLComment(_) | Token.MLComment(_) =>
        Right(state,None)
      case Token.Comma =>
        Right(( State.ArrAfterComma(value, parentOpt), None ))
      case Token.CloseSuqare =>
        parentOpt.acceptArray(state, token, value)
      case _ =>
        Left(s"fail state=$s accept $token")

    // Ожидание или закрытие массива или очередного значения
    case s@State.ArrAfterComma(value,parentOpt) => token match
      case Token.WhiteSpace(_) | Token.SLComment(_) | Token.MLComment(_) =>
        Right(state,None)
      case Token.CloseSuqare =>
        parentOpt.acceptArray(state, token, value)
      case _ =>
        accept( State.ArrExpectValue(value,parentOpt), token )

    case s@State.ObjExpFieldName(value,parentOpt) => token match
      case Token.WhiteSpace(_) | Token.SLComment(_) | Token.MLComment(_) =>
        Right(state,None)
      case Token.CloseBrace =>
        parentOpt.acceptObject(state,token,value)
      case Token.Identifier(text) =>
        Right((
          State.ObjAfterFieldName(text, value, parentOpt),
          None
        ))
      case Token.Str(text) =>
        Right((
          State.ObjAfterFieldName(text, value, parentOpt),
          None
        ))
      case _ =>
        Left(s"fail state=$s accept $token")

    case s@State.ObjAfterFieldName(fieldName,value,parentOpt) => token match
      case Token.WhiteSpace(_) | Token.SLComment(_) | Token.MLComment(_) =>
        Right(state,None)
      case Token.Colon =>
        Right((
          State.ObjExpFieldValue(fieldName,value,parentOpt),
          None
        ))
      case _ =>
        Left(s"fail state=$s accept $token")

    case s@State.ObjExpFieldValue(fieldName,value,parentOpt) => token match
      case Token.WhiteSpace(_) | Token.SLComment(_) | Token.MLComment(_) =>
        Right(state,None)
      case Token.Str(text) => 
        Right(( State.ObjExpectComma(value+(fieldName->AST.JsStr(text))) , None ))
      case Token.IntNumber(num) =>
        Right(( State.ObjExpectComma(value+(fieldName->AST.JsInt(num))) , None ))
      case Token.BigNumber(num) =>
        Right(( State.ObjExpectComma(value+(fieldName->AST.JsBig(num))) , None ))
      case Token.FloatNumber(num) =>
        Right(( State.ObjExpectComma(value+(fieldName->AST.JsFloat(num))) , None ))
      case Token.Identifier(text) => text match
        case "true" => Right(( State.ObjExpectComma(value+(fieldName->AST.JsBool(true))) , None ))
        case "false" => Right(( State.ObjExpectComma(value+(fieldName->AST.JsBool(false))) , None ))
        case "null" => Right(( State.ObjExpectComma(value+(fieldName->AST.JsNull)) , None ))
      case Token.OpenSuqare =>
        Right((
          State.ArrExpectValue(List(),Some(state)),
          None
        ))
      case Token.OpenBrace =>
        Right((
          State.ObjExpFieldName(Map(),Some(state)),
          None
        ))
      case _ =>
        Left(s"fail state=$s accept $token, expect value")

    case s@State.ObjExpectComma(value,parentOpt) => token match
      case Token.Comma =>
        Right((
          State.ObjAfterComma(value,parentOpt),
          None
        ))
      case Token.CloseBrace =>
        parentOpt.acceptObject(state,token,value)
      case _ => 
        Left(s"fail state=$state accept $token")

    case s@State.ObjAfterComma(value,parentOpt) => token match
      case Token.CloseBrace =>
        parentOpt.acceptObject(state,token,value)
      case _ =>
        accept( State.ObjExpFieldName(value,parentOpt), token )
  