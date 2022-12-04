package xyz.cofe.json4s3.query

import xyz.cofe.json4s3.stream.ast.AST
import xyz.cofe.json4s3.query.errors.QueryError
import xyz.cofe.json4s3.errors.JsonError

trait Query:
  def apply(name:String):Query
  def string:Either[errors.QueryError, String]
  def byte:Either[errors.QueryError, Byte]
  def short:Either[errors.QueryError, Short]
  def int:Either[errors.QueryError, Int]
  def long:Either[errors.QueryError, Long]
  def float:Either[errors.QueryError, Float]
  def double:Either[errors.QueryError, Double]
  def bigint:Either[errors.QueryError, BigInt]
  def bool:Either[errors.QueryError, Boolean]
  def isNull:Either[errors.QueryError, Boolean]
  def nullValue:Either[errors.QueryError, Unit]
  def array:Either[errors.QueryError, Seq[AST]]
  def map:Either[errors.QueryError, Map[String,AST]]

extension (ast:AST)
  def query:Query = QuerySet(List(ast))

extension (ast:Either[_ <: JsonError,AST])
  def query:Query = ast match
    case Left(srcError) => NestedErrorQuery(srcError)
    case Right(value) => QuerySet(List(value))
