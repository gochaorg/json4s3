package xyz.cofe.json4s3.errors

import xyz.cofe.json4s3.stream.token.StreamTokenParser
import xyz.cofe.json4s3.stream.token.Token
import xyz.cofe.json4s3.stream.ast.Parser.{State => PState}
import scala.reflect.ClassTag

/** Общий тип для всех ошибок json4s3 */
trait JsonError
object JsonError:
  abstract class NoStackErr( message:String, cause:Throwable ) extends Error(message,cause,true,false)

/** Общий тип для лексического анализа */
sealed trait TokenError extends Throwable with JsonError
case class DidNotMatchExpectInput(message:String) extends JsonError.NoStackErr( message, null ) with TokenError
case class NoInput() extends JsonError.NoStackErr( "no input", null ) with TokenError
case class NotReady(message:String) extends JsonError.NoStackErr( message, null ) with TokenError

object TokenError:
  def notMatchInput[P <: StreamTokenParser[Char]:ClassTag](parser:P, state:parser.STATE, char:Char, expectation:String) =
    DidNotMatchExpectInput(s"did not match expected char '${char}', expectation=$expectation, parser=${summon[ClassTag[P]].runtimeClass.toGenericString()}, state=$state")
  def notReady[P <: StreamTokenParser[Char]:ClassTag](parser:P, state:parser.STATE) =
    NotReady(s"data not ready, parser=${summon[ClassTag[P]].runtimeClass.toGenericString()}, state=$state")

/** Общий тип для парсера */
sealed trait ParserError extends Throwable with JsonError

/** Не совпадает поступившая лексема и текущее состоние парсера */
case class ParserNotMatchInputToken[S <: PState:ClassTag](state:S, token:Token, expect:Option[String]=None) extends 
  JsonError.NoStackErr(s"did not match expected token $token, state=${summon[ClassTag[S]].runtimeClass.toGenericString()}",null) with ParserError

/** Входящая лексема-идентификатор не распознана, актуально когда ожидается true|false|null, а пришла дичь */
case class ParserUndefinedIndentifier(state:PState, token:Token) extends JsonError.NoStackErr("undefined identifier",null) with ParserError

/** Актуально когда лишнаяя закрывающая скобка `}`  `]` */
case class ParentStateNotMatch[S <: PState:ClassTag](state:S, token:Token, expect:Option[String]=None) extends 
  JsonError.NoStackErr(
    s"did not match expected token $token,"+
    s" state=${state.name}"+
    ", parent state="+(state.parentOpt)
  ,null) with ParserError

/** Конец входных данных */
case class ParserNoInput() extends JsonError.NoStackErr("No input",null) with ParserError

/** Общий тип для итераторов по лексемам */
sealed trait TokenIteratorError extends Throwable with JsonError
case class TokenIteratorIOError(err:Throwable) extends JsonError.NoStackErr("Token iterator io error",err) with TokenIteratorError
case class TokenIteratorTokenizer(err:TokenError) extends JsonError.NoStackErr(err.getMessage() ,err) with TokenIteratorError
case class TokenIteratorClosed() extends JsonError.NoStackErr("Iterator closed",null) with TokenIteratorError

/** Общий тип для итераторов по json */
sealed trait ParserIteratorError extends Throwable with JsonError
case class ParserIteratorTokError(err:TokenIteratorError) extends JsonError.NoStackErr(err.getMessage(),err) with ParserIteratorError
case class ParserIteratorParserError(err:ParserError) extends JsonError.NoStackErr(err.getMessage(),err) with ParserIteratorError
case class ParserIteratorClosed() extends JsonError.NoStackErr("Iterator closed",null) with ParserIteratorError
