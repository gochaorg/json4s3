package xyz.cofe.json4s3.query

import xyz.cofe.json4s3.stream.ast.AST
import xyz.cofe.json4s3.query.errors.QueryError
import xyz.cofe.json4s3.errors.JsonError


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

