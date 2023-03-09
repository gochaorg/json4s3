package xyz.cofe.json4s3.derv

import xyz.cofe.json4s3.stream.ast.AST
import xyz.cofe.json4s3.stream.ast.AST._
import errors._

import scala.deriving.*
import scala.compiletime.{erasedValue, summonInline, constValue}
import scala.CanEqual.derived

/** 
 * Получение объекта определенного типа из Json 
 * 
 * см [[FromJsonBuilder]]
 */
trait FromJson[T]:
  def fromJson(j:AST):Either[DervError,T]

inline def summonAllFromJson[T <: Tuple]: List[FromJson[_]] =
inline erasedValue[T] match
  case _: EmptyTuple => Nil
  case _: (t *: ts) => summonInline[FromJson[t]] :: summonAllFromJson[ts]

inline def summonAllDefaultValue[T <: Tuple]: List[DefaultValue[_]] =
inline erasedValue[T] match
  case _: EmptyTuple => Nil
  case _: (t *: ts) => summonInline[DefaultValue[t]] :: summonAllDefaultValue[ts]

inline def labelFromMirror[A](using m:Mirror.Of[A]):String = constValue[m.MirroredLabel]
inline def labelsFrom[A <: Tuple]:List[String] = inline erasedValue[A] match
  case _:EmptyTuple => Nil
  case _:(head *: tail) => 
    constValue[head].toString() :: labelsFrom[tail]

inline def labelsOf[A](using m:Mirror.Of[A]) = labelsFrom[m.MirroredElemLabels]

inline def isOptionals[A <: Tuple]:List[Boolean] = inline erasedValue[A] match
  case _:EmptyTuple => Nil
  case x:(head *: tail) => 
    false :: isOptionals[tail]

object FromJson:
  def builder[A] = FromJsonBuilder.query[A]

  // inline given derived[T](using m: Mirror.Of[T]):FromJson[T] =
  //   val names : List[String] = labelsOf[T]
  //   inline m match
  //     case s: Mirror.SumOf[T]     => derivedSum[T](s,names)
  //     case p: Mirror.ProductOf[T] => derivedProduct[T](p)

  inline given derivedSum[T](using m: Mirror.SumOf[T]):FromJson[T] =
    val names : List[String] = labelsOf[T]
    new FromJson[T] {
      def fromJson(jsonTree: AST): Either[DervError, T] = {
        jsonTree match
          case jsObj@ JsObj(jsFields) =>
            if jsFields.isEmpty
            then Left(FromSumFail(s"empty object $jsObj"))
            else
              val (name,content) = jsFields.head
              val idx = names.indexOf(name)
              if idx<0
              then Left(FromSumFail(s"undefined type name, expect one of $names, but found $name"))
              else
                val fromJsonInstances = summonAllFromJson[m.MirroredElemTypes]
                val fromJson = fromJsonInstances(idx)
                fromJson.asInstanceOf[FromJson[T]].fromJson(content)
          case jsValue =>
            Left(FromSumFail(s"expect JsObj, but accept $jsValue"))
      }
    }

  inline given derivedProduct[A](using n:Mirror.ProductOf[A]):FromJson[A] =
    val elems    = summonAllFromJson[n.MirroredElemTypes]
    val defaults = summonAllDefaultValue[n.MirroredElemTypes]
    val names    = labelsFrom[n.MirroredElemLabels]
    val optionals = isOptionals[n.MirroredElemTypes]
    inline n match
      //case s: Mirror.SumOf[A]     => fromJsonSum(s,elems)
      case p: Mirror.ProductOf[A] => fromJsonPoduct(p,elems,names,defaults,optionals)
    
  def fromJsonSum[T](s:Mirror.SumOf[T], elems:List[FromJson[_]]):FromJson[T] = 
    new FromJson[T]:
      def fromJson(js:AST):Either[DervError,T] =
        Left(FromSumFail(s"fromJsonSum js:$js"))

  def fromJsonPoduct[T](
    p:Mirror.ProductOf[T], 
    elems:List[FromJson[_]], 
    names:List[String],
    defaults:List[DefaultValue[_]],
    optionals:List[Boolean]
  ):FromJson[T] = 
    import xyz.cofe.json4s3.derv.OptionExt._

    new FromJson[T]:
      def fromJson(js:AST):Either[DervError,T] =
        js match
          case obj@JsObj(fields) => 
            val res = names.zip(elems).zip(defaults).map { case ((name,restore),tryDefValue) => 
              obj.get(name) match
                case Some(jsFieldValue) =>
                  restore.asInstanceOf[FromJson[Any]].fromJson(jsFieldValue)
                case None =>
                  //summon[DefaultValue[T]].defaultValue.lift(FieldNotFound(s"field $name not found and default value not defined"))
                  tryDefValue.asInstanceOf[DefaultValue[Any]].defaultValue.lift(FieldNotFound(s"field $name not found and default value not defined $tryDefValue"))
            }.foldLeft(Right(List[Any]()):Either[DervError,List[Any]]){ case (a,vE) => 
              vE.flatMap { v => 
                a.map { l => v :: l }
              }
            }.map { _.reverse }
            .map { ls => 
              val prod:Product = new Product {
                override def productArity: Int = ls.size
                override def productElement(n: Int): Any = ls(n)
                override def canEqual(that: Any): Boolean = false
              }
              prod
            }
            .map { prod => 
              p.fromProduct(prod)
            }
            res
          case _ => Left(TypeCastFail(s"fromJsonPoduct can't fetch from $js"))

  given listFromJson[A:FromJson]:FromJson[List[A]] = selfConsistent.listFromJson
  given long4numOrStr:FromJson[Long] = bignum.long4numOrStr
  given bigInt4numOrStr:FromJson[BigInt] = bignum.bigInt4numOrStr
  given optionFromJson[A:FromJson]:FromJson[Option[A]] = selfConsistent.optionFromJson
  given stringFromJson:FromJson[String] = selfConsistent.stringFromJson
  given booleanFromJson:FromJson[Boolean] = selfConsistent.booleanFromJson
  given byteFromJson:FromJson[Byte] = selfConsistent.byteFromJson
  given shortFromJson:FromJson[Short] = selfConsistent.shortFromJson
  given intFromJson:FromJson[Int] = selfConsistent.intFromJson
  given floatFromJson:FromJson[Float] = selfConsistent.floatFromJson
  given doubleFromJson:FromJson[Double] = selfConsistent.doubleFromJson