package xyz.cofe.json4s3.derv

import xyz.cofe.json4s3.stream.ast.AST
import xyz.cofe.json4s3.stream.ast.AST._
import errors._

import scala.deriving.*
import scala.compiletime.{erasedValue, summonInline, constValue}
import scala.CanEqual.derived

/** Получение объекта определенного типа из Json */
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

object FromJson:
  inline given derived[A](using n:Mirror.Of[A]):FromJson[A] =
    val elems    = summonAllFromJson[n.MirroredElemTypes]
    val defaults = summonAllDefaultValue[n.MirroredElemTypes]
    val names    = labelsFrom[n.MirroredElemLabels]    
    inline n match
      case s: Mirror.SumOf[A]     => fromJsonSum(s,elems)
      case p: Mirror.ProductOf[A] => fromJsonPoduct(p,elems,names,defaults)
    
  def fromJsonSum[T](s:Mirror.SumOf[T], elems:List[FromJson[_]]):FromJson[T] = 
    new FromJson[T]:
      def fromJson(js:AST):Either[DervError,T] =
        Left(FromSumFail(s"fromJsonSum js:$js"))

  def fromJsonPoduct[T](
    p:Mirror.ProductOf[T], 
    elems:List[FromJson[_]], 
    names:List[String],
    defaults:List[DefaultValue[_]]
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
                  tryDefValue.asInstanceOf[DefaultValue[Any]].defaultValue.lift(FieldNotFound(s"field $name not found and default value not defined"))
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

  given FromJson[Double] with
    def fromJson(j:AST) = j match
      case JsInt(n) => Right(n.toDouble)
      case JsFloat(n) => Right(n.toDouble)
      case JsBig(n) => Right(n.toDouble)
      case _ => Left(TypeCastFail(s"can't get double from $j"))
  given FromJson[Float] with
    def fromJson(j:AST) = j match
      case JsInt(n) => Right(n.toFloat)
      case JsFloat(n) => Right(n.toFloat)
      case JsBig(n) => Right(n.toFloat)
      case _ => Left(TypeCastFail(s"can't get float from $j"))
  given FromJson[Byte] with
    def fromJson(j:AST) = j match
      case JsInt(n) => Right(n.toByte)
      case JsFloat(n) => Right(n.toByte)
      case JsBig(n) => Right(n.toByte)
      case _ => Left(TypeCastFail(s"can't get byte from $j"))
  given FromJson[Short] with
    def fromJson(j:AST) = j match
      case JsInt(n) => Right(n.toShort)
      case JsFloat(n) => Right(n.toShort)
      case JsBig(n) => Right(n.toShort)
      case _ => Left(TypeCastFail(s"can't get short from $j"))
  given FromJson[Int] with
    def fromJson(j:AST) = j match
      case JsInt(n) => Right(n)
      case JsFloat(n) => Right(n.toInt)
      case JsBig(n) => Right(n.toInt)
      case _ => Left(TypeCastFail(s"can't get int from $j"))
  given FromJson[Long] with
    def fromJson(j:AST) = j match
      case JsInt(n) => Right(n.toShort)
      case JsFloat(n) => Right(n.toShort)
      case JsBig(n) => Right(n.toShort)
      case _ => Left(TypeCastFail(s"can't get long from $j"))
  given FromJson[BigInt] with
    def fromJson(j:AST) = j match
      case JsInt(n) => Right(BigInt(n))
      case JsFloat(n) => Right(BigInt(n.toLong))
      case JsBig(n) => Right(n)
      case _ => Left(TypeCastFail(s"can't get big int from $j"))
  given FromJson[Boolean] with
    def fromJson(j:AST) = j match
      case JsBool(n) => Right(n)
      case _ => Left(TypeCastFail(s"can't get boolean from $j"))
  given FromJson[String] with
    def fromJson(j:AST) = j match
      case JsStr(n) => Right(n)
      case _ => Left(TypeCastFail(s"can't get string from $j"))
  given [A:FromJson]:FromJson[List[A]] with
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
