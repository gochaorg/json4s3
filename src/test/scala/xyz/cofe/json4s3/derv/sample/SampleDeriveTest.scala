package xyz.cofe.json4s3.derv.sample

import scala.deriving.*
import scala.compiletime.{erasedValue, summonInline, constValue}
import scala.CanEqual.derived

trait Show[T]:
  def show(t:T):String

object Show:
  given Show[Int] with
    def show(t:Int):String = s"$t"

  inline given derived[A](using m:Mirror.Of[A]):Show[A] =
    new Show[A] {
      def show(a:A):String = 
        val t = inline m match
          case s:Mirror.SumOf[A] => "sum"
          case p:Mirror.ProductOf[A] => "product"
        
        s"type=$t"+
        " label="+label[A]+
        " labels="+labelsOf[A]
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
  case Single
  case Other

class SampleDeriveTest extends munit.FunSuite:

  test("tst") {
    println("product?")    
    println( summon[Show[Sample]].show(Sample(1,2)) )

    println("b")
    println( summon[Show[ADT]].show(ADT.Single) )
  }
