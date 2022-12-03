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

case class QuerySet( workSet:List[AST] ) extends Query:
  def apply(name: String): Query = 
    copy( workSet=
      workSet
        .filter { ast => ast.isInstanceOf[AST.JsObj] }
        .map { ast => ast.asInstanceOf[AST.JsObj] }
        .flatMap { jsObj => 
          jsObj.value.flatMap { case (fieldName,ast) => 
            if fieldName==name then List(ast) else List()
          }
        } 
    )

  private def first:Either[QueryError, AST] = workSet.size match
    case 0 => Left(errors.NoDataError())
    case _ if workSet.size>1 => Left(errors.AmbiguousError(workSet))
    case _ => Right(workSet.head)

  def string: Either[QueryError, String] =
    first.flatMap { 
      case AST.JsStr(value) => Right(value)
      case ast => Left( errors.CastError[String](ast) )
    }
  
  def byte: Either[QueryError, Byte] = 
    first.flatMap { 
      case AST.JsInt(value) => Right(value.toByte)
      case AST.JsFloat(value) => Right(value.toByte)
      case AST.JsBig(value) => Right(value.toByte)
      case ast => Left( errors.CastError[Byte](ast) )
    }

  def short: Either[QueryError, Short] = 
    first.flatMap { 
      case AST.JsInt(value) => Right(value.toShort)
      case AST.JsFloat(value) => Right(value.toShort)
      case AST.JsBig(value) => Right(value.toShort)
      case ast => Left( errors.CastError[Byte](ast) )
    }

  def int: Either[QueryError, Int] = 
    first.flatMap { 
      case AST.JsInt(value) => Right(value)
      case AST.JsFloat(value) => Right(value.toInt)
      case AST.JsBig(value) => Right(value.toInt)
      case ast => Left( errors.CastError[Byte](ast) )
    }

  def long: Either[QueryError, Long] = 
    first.flatMap { 
      case AST.JsInt(value) => Right(value.toLong)
      case AST.JsFloat(value) => Right(value.toLong)
      case AST.JsBig(value) => Right(value.toLong)
      case ast => Left( errors.CastError[Byte](ast) )
    }

  def float: Either[QueryError, Float] = 
    first.flatMap { 
      case AST.JsInt(value) => Right(value.toFloat)
      case AST.JsFloat(value) => Right(value.toFloat)
      case AST.JsBig(value) => Right(value.toFloat)
      case ast => Left( errors.CastError[Byte](ast) )
    }

  def double: Either[QueryError, Double] = 
    first.flatMap { 
      case AST.JsInt(value) => Right(value.toDouble)
      case AST.JsFloat(value) => Right(value.toDouble)
      case AST.JsBig(value) => Right(value.toDouble)
      case ast => Left( errors.CastError[Byte](ast) )
    }

  def bigint: Either[QueryError, BigInt] = 
    first.flatMap { 
      case AST.JsInt(value) => Right(BigInt(value))
      case AST.JsFloat(value) => Right(BigInt(value.toString()))
      case AST.JsBig(value) => Right(value)
      case ast => Left( errors.CastError[Byte](ast) )
    }

  def bool:Either[errors.QueryError, Boolean] = 
    first.flatMap { 
      case AST.JsBool(value) => Right(value)
      case ast => Left( errors.CastError[Boolean](ast) )
    }

  def isNull:Either[errors.QueryError, Boolean] =
    first.flatMap { 
      case AST.JsNull => Right(true)
      case ast => Right(false)
    }

  def nullValue:Either[errors.QueryError, Unit] = 
    first.flatMap { 
      case AST.JsNull => Right(())
      case ast => Left( errors.CastError(s"not null value, source value is ${ast}") )
    }

  def array:Either[errors.QueryError, Seq[AST]] =
    Right(workSet)

  def map:Either[errors.QueryError, Map[String,AST]] =
    first.flatMap { 
      case AST.JsObj(fields) => Right(fields.toMap)
      case ast => Left( errors.CastError(s"expect AST.JsObj, source value is ${ast}") )
    }
