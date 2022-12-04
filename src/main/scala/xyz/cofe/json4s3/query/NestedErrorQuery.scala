package xyz.cofe.json4s3.query

import xyz.cofe.json4s3.stream.ast.AST
import xyz.cofe.json4s3.query.errors.QueryError
import xyz.cofe.json4s3.errors.JsonError

case class NestedErrorQuery(sourceError:JsonError) extends Query:
  def apply(name:String):Query = this
  def string:Either[errors.QueryError, String] = Left(errors.NestedError(sourceError))
  def byte:Either[errors.QueryError, Byte] = Left(errors.NestedError(sourceError))
  def short:Either[errors.QueryError, Short] = Left(errors.NestedError(sourceError))
  def int:Either[errors.QueryError, Int] = Left(errors.NestedError(sourceError))
  def long:Either[errors.QueryError, Long] = Left(errors.NestedError(sourceError))
  def float:Either[errors.QueryError, Float] = Left(errors.NestedError(sourceError))
  def double:Either[errors.QueryError, Double] = Left(errors.NestedError(sourceError))
  def bigint:Either[errors.QueryError, BigInt] = Left(errors.NestedError(sourceError))
  def bool:Either[errors.QueryError, Boolean] = Left(errors.NestedError(sourceError))
  def isNull:Either[errors.QueryError, Boolean] = Left(errors.NestedError(sourceError))
  def nullValue:Either[errors.QueryError, Unit] = Left(errors.NestedError(sourceError))
  def array:Either[errors.QueryError, Seq[AST]] = Left(errors.NestedError(sourceError))
  def map:Either[errors.QueryError, Map[String,AST]] = Left(errors.NestedError(sourceError))

