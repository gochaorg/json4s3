package xyz.cofe.json4s3.errors

import xyz.cofe.json4s3.stream.token.StreamTokenParser
import scala.reflect.ClassTag

sealed trait JsonError
object JsonError:
  abstract class NoStackErr( message:String, cause:Throwable ) extends Error(message,cause,true,false)

sealed trait TokenError extends JsonError
object TokenError:
  case class DidNotMatchExpectInput(message:String) extends JsonError.NoStackErr( message, null ) with TokenError

  def notMatchInput[P <: StreamTokenParser[Char]:ClassTag](parser:P, state:parser.STATE, char:Char, expectation:String) =
    DidNotMatchExpectInput(s"did not match expected char '${char}', expectation=$expectation, parser=${summon[ClassTag[P]].runtimeClass.toGenericString()}, state=$state")

  case class NoInput() extends JsonError.NoStackErr( "no input", null ) with TokenError

  case class NotReady(message:String) extends JsonError.NoStackErr( message, null ) with TokenError

  def notReady[P <: StreamTokenParser[Char]:ClassTag](parser:P, state:parser.STATE) =
    NotReady(s"data not ready, parser=${summon[ClassTag[P]].runtimeClass.toGenericString()}, state=$state")