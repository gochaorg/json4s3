package xyz.cofe.json4s3.derv
package bignum

import xyz.cofe.json4s3.stream.ast.AST
import xyz.cofe.json4s3.stream.ast.AST.*
import xyz.cofe.json4s3.derv.errors.*
import scala.util.*

// Работа с типами JVM/Scala которые напрямую не совпадают с JSON

/* #region Long */

given long2jsBig:ToJson[Long] with
  def toJson(t: Long): Option[AST] = Some(AST.JsBig(t))

given long2jsStr:ToJson[Long] with
  override def toJson(t: Long): Option[AST] = Some(AST.JsStr(t.toString()))

given long4numOrStr:FromJson[Long] with
  def fromJson(j:AST) = j match
    case JsStr(str) if str.matches("-?\\d+") => str.toLongOption.map(n => Right(n)).getOrElse(Left(TypeCastFail(s"can't parse '${str}'' as Long")))
    case JsInt(n) => Right(n.toLong)
    case JsFloat(n) => Right(n.toLong)
    case JsBig(n) => Right(n.toLong)
    case _ => Left(TypeCastFail(s"can't get long from $j"))

/* #endregion */
/* #region BigInt */

given bigInt2jsBig:ToJson[BigInt] with
  def toJson(t: BigInt): Option[AST] = Some(AST.JsBig(t))

given bigInt2jsStr:ToJson[BigInt] with
  def toJson(t: BigInt): Option[AST] = Some(AST.JsStr(t.toString()))

given bigInt4numOrStr:FromJson[BigInt] with
  def fromJson(j:AST) = j match
    case JsStr(str) if str.matches("-?\\d+") => 
      Try(BigInt(str)) match
        case Success(num) => Right(num)
        case Failure(err) => Left(TypeCastFail(s"can't parse '${str}'' as BigInt"))
    case JsInt(n) => Right(BigInt(n))
    case JsFloat(n) => Right(BigInt(n.toLong))
    case JsBig(n) => Right(n)
    case _ => Left(TypeCastFail(s"can't get BigInt from $j"))

/* #endregion */

trait StringImplToJson:
  given long2jsStr:ToJson[Long] = bignum.long2jsStr
  given bigInt2jsStr:ToJson[BigInt] = bignum.bigInt2jsStr

object StringImplToJson extends StringImplToJson

trait JsImplToJson:
  given long2jsBig:ToJson[Long] = bignum.long2jsBig
  given bigInt2jsBig:ToJson[BigInt] = bignum.bigInt2jsBig

object JsImplToJson extends JsImplToJson

trait BigNumFromJson:
  given long4numOrStr:FromJson[Long] = bignum.long4numOrStr
  given bigInt4numOrStr:FromJson[BigInt] = bignum.bigInt4numOrStr

object BigNumFromJson extends BigNumFromJson

object StringImpl extends StringImplToJson with BigNumFromJson
object JsImpl extends JsImplToJson with BigNumFromJson