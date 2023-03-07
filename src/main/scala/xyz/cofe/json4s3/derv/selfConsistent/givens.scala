package xyz.cofe.json4s3.derv
package selfConsistent

import xyz.cofe.json4s3.stream.ast.AST
import xyz.cofe.json4s3.stream.ast.AST.*
import xyz.cofe.json4s3.derv.errors.*

// Файл с типами которые совпадают полностью с JSON
// Исключение по:
//   Long - оно 64 бита и может двояко быть интерпретировано со стороны JSON
//          обычно значение засовывают в String
//   BigInt - в стандарте JSON его нет, есть в JS, в версии которая появилась 
//            после стандартизации JSON

/* #region Double */

given doubleFromJson:FromJson[Double] with
  def fromJson(j:AST) = j match
    case JsInt(n) => Right(n.toDouble)
    case JsFloat(n) => Right(n.toDouble)
    case JsBig(n) => Right(n.toDouble)
    case _ => Left(TypeCastFail(s"can't get double from $j"))

given doubleToJson:ToJson[Double] with
  def toJson(t: Double): Option[AST] = Some(AST.JsFloat(t))

/* #endregion */
/* #region Float */

given floatFromJson:FromJson[Float] with
  def fromJson(j:AST) = j match
    case JsInt(n) => Right(n.toFloat)
    case JsFloat(n) => Right(n.toFloat)
    case JsBig(n) => Right(n.toFloat)
    case _ => Left(TypeCastFail(s"can't get float from $j"))

given floatToJson:ToJson[Float] with
  def toJson(t: Float): Option[AST] = Some(AST.JsFloat(t))

/* #endregion */
/* #region Byte */

given byteFromJson:FromJson[Byte] with
  def fromJson(j:AST) = j match
    case JsInt(n) => Right(n.toByte)
    case JsFloat(n) => Right(n.toByte)
    case JsBig(n) => Right(n.toByte)
    case _ => Left(TypeCastFail(s"can't get byte from $j"))

given byteToJson:ToJson[Byte] with
  def toJson(t: Byte): Option[AST] = Some(AST.JsInt(t))

/* #endregion */
/* #region Short */

given shortFromJson:FromJson[Short] with
  def fromJson(j:AST) = j match
    case JsInt(n) => Right(n.toShort)
    case JsFloat(n) => Right(n.toShort)
    case JsBig(n) => Right(n.toShort)
    case _ => Left(TypeCastFail(s"can't get short from $j"))

given shortToJson:ToJson[Short] with
  def toJson(t: Short): Option[AST] = Some(AST.JsInt(t))

/* #endregion */
/* #region Int */

given intFromJson:FromJson[Int] with
  def fromJson(j:AST) = j match
    case JsInt(n) => Right(n)
    case JsFloat(n) => Right(n.toInt)
    case JsBig(n) => Right(n.toInt)
    case _ => Left(TypeCastFail(s"can't get int from $j"))

given intToJson:ToJson[Int] with
  def toJson(t: Int): Option[AST] = Some(AST.JsInt(t))

/* #endregion */
/* #region Boolean */

given booleanFromJson:FromJson[Boolean] with
  def fromJson(j:AST) = j match
    case JsBool(n) => Right(n)
    case _ => Left(TypeCastFail(s"can't get boolean from $j"))

given booleanToJson:ToJson[Boolean] with
  def toJson(t: Boolean): Option[AST] = Some(AST.JsBool(t))

/* #endregion */
/* #region String */

given stringFromJson:FromJson[String] with
  def fromJson(j:AST) = j match
    case JsStr(n) => Right(n)
    case _ => Left(TypeCastFail(s"can't get string from $j"))

given stringToJson:ToJson[String] with
  def toJson(t: String): Option[AST] = Some(AST.JsStr(t))

/* #endregion */
/* #region List */

given listFromJson[A:FromJson]:FromJson[List[A]] with
  def fromJson(js:AST) = js match
    case JsArray(array) =>
      val item = summon[FromJson[A]]
      array.map { a => item.fromJson(a) }.foldLeft( 
        Right(List[A]()):Either[DervError,List[A]] 
      ){ case(sum,itmEt) => 
        sum.flatMap { lst => 
          itmEt.map { itm =>
            lst :+ itm
          }
        }
      }
    case _ => Left(TypeCastFail(s"can't get List[A] from $js"))

given listToJson[A:ToJson]:ToJson[List[A]] with
  def toJson(list: List[A]): Option[AST] =
    val item2json = summon[ToJson[A]]
    val l = list.map(a => item2json.toJson(a)).flatten
    Some(JsArray(l))

/* #endregion */
/* #region Option */

given optionFromJson[A:FromJson]:FromJson[Option[A]] with
  def fromJson(j:AST) = summon[FromJson[A]].fromJson(j).map(Some(_))

given optionToJson[A:ToJson]:ToJson[Option[A]] with
  def toJson(itm:Option[A]) = itm.flatMap(it=>summon[ToJson[A]].toJson(it))

/* #endregion */

trait ConsistentFromJson:
  given doubleFromJson:FromJson[Double] = selfConsistent.doubleFromJson
  given floatFromJson:FromJson[Float] = selfConsistent.floatFromJson
  given intFromJson:FromJson[Int] = selfConsistent.intFromJson
  given shortFromJson:FromJson[Short] = selfConsistent.shortFromJson
  given byteFromJson:FromJson[Byte] = selfConsistent.byteFromJson
  given booleanFromJson:FromJson[Boolean] = selfConsistent.booleanFromJson
  given stringFromJson:FromJson[String] = selfConsistent.stringFromJson
  given listFromJson[A:FromJson]:FromJson[List[A]] = selfConsistent.listFromJson
  given optionFromJson[A:FromJson]:FromJson[Option[A]] = selfConsistent.optionFromJson

trait ConsistentToJson:
  given doubleToJson:ToJson[Double] = selfConsistent.doubleToJson
  given floatToJson:ToJson[Float] = selfConsistent.floatToJson
  given intToJson:ToJson[Int] = selfConsistent.intToJson
  given shortToJson:ToJson[Short] = selfConsistent.shortToJson
  given byteToJson:ToJson[Byte] = selfConsistent.byteToJson
  given booleanToJson:ToJson[Boolean] = selfConsistent.booleanToJson
  given stringToJson:ToJson[String] = selfConsistent.stringToJson
  given listToJson[A:ToJson]:ToJson[List[A]] = selfConsistent.listToJson
  given optionToJson[A:ToJson]:ToJson[Option[A]] = selfConsistent.optionToJson
