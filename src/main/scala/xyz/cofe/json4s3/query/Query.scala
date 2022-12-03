package xyz.cofe.json4s3.query

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

