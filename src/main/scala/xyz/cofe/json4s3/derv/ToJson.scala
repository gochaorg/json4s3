package xyz.cofe.json4s3.derv

import xyz.cofe.json4s3.stream.ast.AST
import xyz.cofe.json4s3.stream.ast.AST._
import errors._

import scala.deriving.*
import scala.compiletime._
import scala.CanEqual.derived

inline def summonAllToJson[T <: Tuple]: List[ToJson[_]] =
  inline erasedValue[T] match
    case _: EmptyTuple => Nil
    case _: (t *: ts) => summonInline[ToJson[t]] :: summonAllToJson[ts]

trait ToJson[T]:
  def toJson(t:T):Option[AST]

object ToJson:
  // https://dotty.epfl.ch/docs/reference/contextual/derivation.html
  def iterator[T](p: T) = p.asInstanceOf[Product].productIterator

  inline given derived[T](using m: scala.deriving.Mirror.Of[T]): ToJson[T] = 
    val elems2json = summonAllToJson[m.MirroredElemTypes]
    inline m match
      //case s: Mirror.SumOf[T] => toJsonSum(s, elems, labelsOf[T])
      case p: Mirror.ProductOf[T] => toJsonProduct(p, elems2json)
  
  // def toJsonSum[T](s: Mirror.SumOf[T], elems: List[ToJson[_]], names:List[String]):ToJson[T] = 
  //   new ToJson[T]:
  //     def toJson(t:T):AST =
  //       val nameIdx = s.ordinal(t)
  //       val rr = elems.map { tjs => tjs.asInstanceOf[ToJson[Any]].toJson(t) }
  //       throw ToSumFail(s"toJsonSum s:$s elems:$elems t:$t rr:$rr names:$names nameIdx=$nameIdx")

  def toJsonProduct[T](
    p: Mirror.ProductOf[T], 
    elems2json: List[ToJson[_]],
  ):ToJson[T] = 
    new ToJson[T]:
      def toJson(t:T):Option[AST] =
        val fields = t.asInstanceOf[Product]
          .productIterator
          .zip(elems2json)
          .zip( t.asInstanceOf[Product].productElementNames)
          .map { case(((value,value2json),fieldName)) => 
            fieldName -> value2json.asInstanceOf[ToJson[Any]].toJson(value) 
          }
          .filter(_._2.isDefined)
          .map { case(n,vOpt)=> (n,vOpt.get) }
          .toList

        Some(JsObj( fields ))

  given [A:ToJson]:ToJson[Option[A]] with
    def toJson(itm:Option[A]) = itm.flatMap(it=>summon[ToJson[A]].toJson(it))

  given ToJson[Byte] with
    def toJson(t: Byte): Option[AST] = Some(AST.JsInt(t))

  given ToJson[Short] with
    def toJson(t: Short): Option[AST] = Some(AST.JsInt(t))

  given ToJson[Int] with
    def toJson(t: Int): Option[AST] = Some(AST.JsInt(t))

  given ToJson[Long] with
    def toJson(t: Long): Option[AST] = Some(AST.JsBig(t))

  given ToJson[BigInt] with
    def toJson(t: BigInt): Option[AST] = Some(AST.JsBig(t))

  given ToJson[Float] with
    def toJson(t: Float): Option[AST] = Some(AST.JsFloat(t))

  given ToJson[Double] with
    def toJson(t: Double): Option[AST] = Some(AST.JsFloat(t))

  given ToJson[Boolean] with
    def toJson(t: Boolean): Option[AST] = Some(AST.JsBool(t))

  given ToJson[String] with
    def toJson(t: String): Option[AST] = Some(AST.JsStr(t))

  given [A:ToJson]:ToJson[List[A]] with
    def toJson(list: List[A]): Option[AST] =
      val item2json = summon[ToJson[A]]
      val l = list.map(a => item2json.toJson(a)).flatten
      Some(JsArray(l))
