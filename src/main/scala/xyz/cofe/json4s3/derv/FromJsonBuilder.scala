package xyz.cofe.json4s3.derv

import xyz.cofe.json4s3.query.Query
import xyz.cofe.json4s3.query.errors
import xyz.cofe.json4s3.derv.{ errors => derr }
import xyz.cofe.json4s3.stream.ast.AST
import xyz.cofe.json4s3.derv.errors.DervError
import xyz.cofe.json4s3.query.QuerySet

object FromJsonBuilder:
  def query[A]:Selector[A] = Selector()

  case class Selector[A]( queries:List[(Query=>Boolean, FromJson[A])] = List() ):
    def select[B<:A:FromJson](query:Query=>Boolean):Selector[A] = 
      val x = summon[FromJson[B]]
      copy(
        queries = queries :+ 
          ( query
          , x.asInstanceOf[FromJson[A]]
          )
      )
    def build:FromJson[A] = 
      new FromJson[A] {
        override def fromJson(j: AST): Either[DervError, A] = {
          var query = new QuerySet(List(j))
          queries.map { case (q,fj) => ( q(query), fj ) }.find{ case (t,fj) => t }.map( _._2 ) match
            case None => Left(derr.NotMatched())
            case Some(value) => value.fromJson(j)
        }
      }
    // def map[B:FromJson] = new Mapper[A,B]()

  extension [A,B](query:Either[errors.QueryError,String])
    def ===( value:String ):Boolean = 
      query match
        case Left(err) => false
        case Right(str) => str==value
      

  // class Mapper[A,B]( implicit f:FromJson[B] ){
  //   def query:Query = ???
  // }
