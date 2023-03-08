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

  inline def label[A](using m:Mirror.Of[A]):String = constValue[m.MirroredLabel]

  inline def labels[A<:Tuple]:List[String] =
    inline erasedValue[A] match
      case _:EmptyTuple => Nil
      case _:(head *: tail) =>
        constValue[head].toString() :: labels[tail]

  inline def labelsOf[A](using m:Mirror.Of[A]):List[String] = labels[m.MirroredElemLabels]

  // inline given derived[T](using m: Mirror.SumOf[T]):ToJson[T] = 
  //   val names : List[String] = labelsOf[T]
  //   inline m match
  //     case s: Mirror.SumOf[T]     => derivedSum[T](s, names)
  //     case p: Mirror.ProductOf[T] => derivedProduct[T](p)

  inline given derivedSum[T](using m: Mirror.SumOf[T]):ToJson[T] = 
    val names : List[String] = labelsOf[T]
    new ToJson[T] {
      override def toJson(t: T): Option[AST] = {
        // Допустим есть такие типы
        // enum SType:
        //   case Sym
        //   case One(a:Int)
        //   case Two(a:Int,b:String)

        // Имена проиводных типов от базового
        // В данном случае List("Sym", "One", "Two")
        // val names : List[String] = labelsOf[T]

        // Указывает на конкретный экземпляр типа
        // В жанном случае может принимать значения 
        //   0 - для Sym
        //   1 - для One
        //   2 - для Two
        val ord : Int = m.ordinal(t)
        val typeName = names(ord)
        val toJsonValues = summonAllToJson[m.MirroredElemTypes]        
        val toValue = toJsonValues(ord)

        // Извлекаем параметры для конструирования значния
        val constructParams = toValue.asInstanceOf[ToJson[Any]].toJson(t)        

        constructParams.map { value => 
          JsObj(List(typeName -> value))
        }
      }
    }

  inline given derivedProduct[T](using m: Mirror.ProductOf[T]): ToJson[T] = 
    val elems2json = summonAllToJson[m.MirroredElemTypes]
    toJsonProduct(m, elems2json)

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

  given optionToJson[A:ToJson]:ToJson[Option[A]] = selfConsistent.optionToJson
  given listToJson[A:ToJson]:ToJson[List[A]] = selfConsistent.listToJson
  given doubleToJson:ToJson[Double] = selfConsistent.doubleToJson
  given floatToJson:ToJson[Float] = selfConsistent.floatToJson
  given intToJson:ToJson[Int] = selfConsistent.intToJson
  given shortToJson:ToJson[Short] = selfConsistent.shortToJson
  given byteToJson:ToJson[Byte] = selfConsistent.byteToJson
  given booleanToJson:ToJson[Boolean] = selfConsistent.booleanToJson
  given stringToJson:ToJson[String] = selfConsistent.stringToJson
  given long2jsStr:ToJson[Long] = bignum.long2jsStr
  given bigInt2jsStr:ToJson[BigInt] = bignum.bigInt2jsStr
