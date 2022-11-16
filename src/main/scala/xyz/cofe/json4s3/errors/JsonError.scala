package xyz.cofe.json4s3.errors

import xyz.cofe.json4s3.stream.token.StreamTokenParser
import xyz.cofe.json4s3.stream.token.Token
import xyz.cofe.json4s3.stream.ast.Parser.{State => PState}
import scala.reflect.ClassTag

sealed trait JsonError
object JsonError:
  abstract class NoStackErr( message:String, cause:Throwable ) extends Error(message,cause,true,false)

sealed trait TokenError extends JsonError
case class DidNotMatchExpectInput(message:String) extends JsonError.NoStackErr( message, null ) with TokenError
case class NoInput() extends JsonError.NoStackErr( "no input", null ) with TokenError
case class NotReady(message:String) extends JsonError.NoStackErr( message, null ) with TokenError

object TokenError:
  def notMatchInput[P <: StreamTokenParser[Char]:ClassTag](parser:P, state:parser.STATE, char:Char, expectation:String) =
    DidNotMatchExpectInput(s"did not match expected char '${char}', expectation=$expectation, parser=${summon[ClassTag[P]].runtimeClass.toGenericString()}, state=$state")
  def notReady[P <: StreamTokenParser[Char]:ClassTag](parser:P, state:parser.STATE) =
    NotReady(s"data not ready, parser=${summon[ClassTag[P]].runtimeClass.toGenericString()}, state=$state")

sealed trait ParserError extends JsonError

case class ParserNotMatchInputToken[S <: PState:ClassTag](state:S, token:Token, expect:Option[String]=None) extends 
  JsonError.NoStackErr(s"did not match expected token $token, state=${summon[ClassTag[S]].runtimeClass.toGenericString()}",null) with ParserError

case class ParserUndefinedIndentifier(state:PState, token:Token) extends JsonError.NoStackErr("",null) with ParserError

case class ParentStateNotMatch[S <: PState:ClassTag](state:S, token:Token, expect:Option[String]=None) extends 
  JsonError.NoStackErr(
    s"did not match expected token $token,"+
    s" state=${state.name}"+
    ", parent state="+(state.parentOpt)
  ,null) with ParserError

case class ParserNoInput() extends JsonError.NoStackErr("No input",null) with ParserError
