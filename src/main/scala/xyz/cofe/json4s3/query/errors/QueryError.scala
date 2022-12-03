package xyz.cofe.json4s3.query.errors

import xyz.cofe.json4s3.errors.JsonError
import xyz.cofe.json4s3.stream.ast.AST
import scala.reflect.ClassTag

sealed trait QueryError extends JsonError

case class NoDataError() 
  extends JsonError.NoStackErr("Workset is empty",null) with QueryError
case class AmbiguousError(workSet:List[AST]) 
  extends JsonError.NoStackErr("Ambiguous read data",null) with QueryError

case class CastError(message:String) 
  extends JsonError.NoStackErr(message, null)
  with QueryError

object CastError:
  def apply[TARGET:ClassTag](ast:AST):CastError = new CastError(s"Can't cast to ${summon[ClassTag[TARGET]].runtimeClass.getSimpleName()} from ${ast}")
  def apply(message:String) = new CastError(message)

case class NestedError( cause:JsonError )
  extends JsonError.NoStackErr(cause.getMessage(), cause)
  with QueryError