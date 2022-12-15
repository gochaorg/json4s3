package xyz.cofe.json4s3.derv.sample

import scala.deriving.*
import scala.compiletime.{erasedValue, summonInline, constValue, summonAll}
import scala.CanEqual.derived

trait Show[T]:
  def show(t:T):String

object Show:
  given Show[Int] with
    def show(t:Int):String = s"$t"

  inline def summonAllShow[T <: Tuple]:List[Show[_]] =
    inline erasedValue[T] match
      case _:EmptyTuple => Nil
      case _:(head *: tail) => summonInline[Show[head]] :: summonAllShow[tail]

  inline given derived[A](using m:Mirror.Of[A]):Show[A] =
    new Show[A] {
      def show(a:A):String = 
        val t = inline m match
          case s:Mirror.SumOf[A] => "sum"
          case p:Mirror.ProductOf[A] => "product"  

        val elemShows = summonAllShow[m.MirroredElemTypes]
        val values =
          inline m match
            case p:Mirror.ProductOf[A] =>
              ((labelsOf[A] zip a.asInstanceOf[Product].productIterator) zip elemShows)
                .map { case((name,value),showValue) => 
                  name -> showValue.asInstanceOf[Show[Any]].show(value)
                }
            case s:Mirror.SumOf[A] =>
              val ord = s.ordinal(a)
              List(
                "ord"->ord,
                "show"->elemShows(ord).asInstanceOf[Show[Any]].show(a)
              )
        
        s"type=$t"+
        " label="+label[A]+
        " labels="+labelsOf[A]+
        //" elemShows="+elemShows+
        " values="+values.toMap
    }

  inline def label[A](using m:Mirror.Of[A]):String = constValue[m.MirroredLabel]
  inline def labelsOf[A](using m:Mirror.Of[A]) = labels[m.MirroredElemLabels]
  inline def labels[A<:Tuple]:List[String] =
    inline erasedValue[A] match
      case _:EmptyTuple => Nil
      case _:(head *: tail) =>
        constValue[head].toString() :: labels[tail]
    

case class Sample(a:Int, b:Int)
enum ADT:
  case Simbol
  case One(a:Int)
  case Two(a:Int,b:Int)

class SampleDeriveTest extends munit.FunSuite:

  test("tst") {
    println( "Sample" )
    println( summon[Show[Sample]].show(Sample(1,2)) )
    println( "ADT.Simbol" )
    println( summon[Show[ADT.Simbol.type]].show(ADT.Simbol) )
    println( "ADT" )
    println( summon[Show[ADT]].show(ADT.Simbol) )
    println( "ADT.One" )
    println( summon[Show[ADT.One]].show(ADT.One(3)) )
    println( "ADT" )
    println( summon[Show[ADT]].show(ADT.One(3)) )
  }
