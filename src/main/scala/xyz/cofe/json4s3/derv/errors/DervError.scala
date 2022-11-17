package xyz.cofe.json4s3.derv.errors

import xyz.cofe.json4s3.errors.JsonError
import xyz.cofe.json4s3.errors.TokenError
import xyz.cofe.json4s3.errors.ParserError

sealed trait DervError extends JsonError
object DervError:
  def from(error:TokenError):DervError = NestedTokenError(error)
  def from(error:ParserError):DervError = NestedParserError(error)

case class TypeCastFail(message:String) extends JsonError.NoStackErr(message,null) with DervError
case class FromSumFail(message:String) extends JsonError.NoStackErr(message,null) with DervError
case class FieldNotFound(message:String) extends JsonError.NoStackErr(message,null) with DervError
case class NestedTokenError(error:TokenError) extends JsonError.NoStackErr(error.getMessage(),error) with DervError
case class NestedParserError(error:ParserError) extends JsonError.NoStackErr(error.getMessage(),error) with DervError