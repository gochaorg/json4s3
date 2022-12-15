package xyz.cofe.json4s3.derv

import xyz.cofe.json4s3.stream.ast.AST
import xyz.cofe.json4s3.stream.ast.AST._
import scala.deriving.*
import scala.compiletime._
import xyz.cofe.json4s3.derv.errors._
import xyz.cofe.json4s3.stream.ast.AST

enum SType:
  case Sym
  case One(a:Int)
  case Two(a:Int,b:String)

class SumTypeTest extends munit.FunSuite:
  test("One(1) to json") {
    println("One(1) to json")
    println("="*60)

    given der : ToJson[SType] = SumTypeTest.derivedToJson[SType]

    val v : SType = SType.One(1)
    val jsonTreeOpt =  summon[ToJson[SType]].toJson(v)
    println( jsonTreeOpt.map(_.string).getOrElse("?") )

    assert(jsonTreeOpt.isDefined)
    
    val jsonTree = jsonTreeOpt.get
    assert(jsonTree == JsObj(List("One" -> JsObj(List("a" -> JsInt(1))))))
  }

  test("json to One") {
    println("json to One")
    println("="*60)

    given der : FromJson[SType] = SumTypeTest.derivedFromJson[SType]

    val resultEither = """{"One":{"a":1}}""".jsonAs[SType]
    println( resultEither )

    assert( resultEither == Right(SType.One(1)) )
  }

  test("Two(1,2) to json") {
    println("Two(1,2) to json")
    println("="*60)
    given der : ToJson[SType] = SumTypeTest.derivedToJson[SType]

    val v : SType = SType.Two(1,"abc")
    println( summon[ToJson[SType]].toJson(v).map(_.string).getOrElse("?") )
  }

object SumTypeTest:
  inline def label[A](using m:Mirror.Of[A]):String = constValue[m.MirroredLabel]

  inline def labels[A<:Tuple]:List[String] =
    inline erasedValue[A] match
      case _:EmptyTuple => Nil
      case _:(head *: tail) =>
        constValue[head].toString() :: labels[tail]

  inline def labelsOf[A](using m:Mirror.Of[A]):List[String] = labels[m.MirroredElemLabels]

  inline given derivedToJson[T](using m: scala.deriving.Mirror.SumOf[T]): ToJson[T] =
    new ToJson[T] {
      override def toJson(t: T): Option[AST] = {
        // Имена проиводных типов от базового
        // В данном случае List("Sym", "One", "Two")
        val names : List[String] = labelsOf[T] 

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

        //JsObj(List(typeName))
        //Some(JsStr(s"sum name=$typeName value=${constructParams}"))

        constructParams.map { value => 
          JsObj(List(typeName -> value))
        }
      }
    }

  inline given derivedFromJson[T](using m: scala.deriving.Mirror.SumOf[T]):FromJson[T] =
    new FromJson[T] {
      def fromJson(jsonTree: AST): Either[DervError, T] = {
        val names : List[String] = labelsOf[T]
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